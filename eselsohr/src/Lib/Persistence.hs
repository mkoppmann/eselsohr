module Lib.Persistence
  ( persistenceApp
  ) where

import qualified Data.HashMap.Strict           as Map
import           Lib.App                        ( AppEnv
                                                , pattern E
                                                , MaxConcurrentWrites
                                                , WithError
                                                , WithLog
                                                , WriteQueue
                                                , grab
                                                , log
                                                , runAppLogIO_
                                                )
import           Lib.Core.Domain                ( Id
                                                , Resource
                                                , SynchronizedStoreEvent(..)
                                                )
import           Lib.Impl.Repository.File       ( WithFile
                                                , save
                                                )
import           UnliftIO.Async                 ( mapConcurrently_
                                                , pooledMapConcurrentlyN_
                                                , race_
                                                )
import           UnliftIO.STM                   ( TQueue
                                                , isEmptyTQueue
                                                , newTQueue
                                                , readTQueue
                                                , retrySTM
                                                , tryReadTQueue
                                                , writeTQueue
                                                )

server :: (WithError m, WithLog env m, WithFile env m) => m ()
server = do
  queue                <- grab @WriteQueue
  mMaxConcurrentWrites <- grab @(Maybe MaxConcurrentWrites)
  writeQueuesVar       <- newTVarIO Map.empty
  race_ (fetchUpdates queue writeQueuesVar)
        (processUpdates writeQueuesVar mMaxConcurrentWrites)
  log E "Persistence server finished unexpectedly"
 where
  fetchUpdates
    :: (WithError m, WithFile env m)
    => TQueue SynchronizedStoreEvent
    -> TVar (HashMap (Id Resource) (TQueue SynchronizedStoreEvent))
    -> m Void
  fetchUpdates queue writeQueuesVar = infinitely $ atomically $ do
    update <- readTQueue queue
    addToMap (syncStoreResId update) update writeQueuesVar

  addToMap
    :: Id Resource
    -> SynchronizedStoreEvent
    -> TVar (HashMap (Id Resource) (TQueue SynchronizedStoreEvent))
    -> STM ()
  addToMap resId update writeQueuesVar = do
    writeVarsMap <- readTVar writeQueuesVar
    case Map.lookup resId writeVarsMap of
      Just workerQueue -> writeTQueue workerQueue update
      Nothing          -> do
        newWriteQueue <- newTQueue
        writeTQueue newWriteQueue update
        modifyTVar' writeQueuesVar $ Map.insert resId newWriteQueue

  cleanupMap
    :: TVar (HashMap (Id Resource) (TQueue SynchronizedStoreEvent)) -> STM ()
  cleanupMap writeQueuesVar =
    writeTVar writeQueuesVar =<< filterByActiveQueues writeQueuesVar

  processUpdates
    :: (WithError m, WithLog env m, WithFile env m)
    => TVar (HashMap (Id Resource) (TQueue SynchronizedStoreEvent))
    -> Maybe MaxConcurrentWrites
    -> m Void
  processUpdates writeQueuesVar mMaxConcurrentWrites = infinitely $ do
    todoQueues <- atomically $ do
      activeList <- filterByActiveQueues writeQueuesVar
      if not $ null activeList then pure activeList else retrySTM
    case mMaxConcurrentWrites of
      Nothing        -> mapConcurrently_ worker todoQueues
      Just maxWrites -> pooledMapConcurrentlyN_ maxWrites worker todoQueues
    atomically $ cleanupMap writeQueuesVar

  filterByActiveQueues
    :: TVar (HashMap (Id Resource) (TQueue SynchronizedStoreEvent))
    -> STM (HashMap (Id Resource) (TQueue SynchronizedStoreEvent))
  filterByActiveQueues =
    readTVar
      >=> Map.toList
      >>> filterM (pure . not <=< isEmptyTQueue . snd)
      >=> Map.fromList
      >>> pure

worker
  :: (WithError m, WithLog env m, WithFile env m)
  => TQueue SynchronizedStoreEvent
  -> m ()
worker queue = join . atomically $ tryReadTQueue queue >>= \case
  Nothing     -> pure pass
  Just update -> pure $ storeUpdate update >> worker queue

storeUpdate
  :: (WithError m, WithLog env m, WithFile env m)
  => SynchronizedStoreEvent
  -> m ()
storeUpdate SynchronizedStoreEvent {..} =
  save syncStoreResId syncStoreEvents >> atomically (putTMVar syncVar ())

persistenceApp :: AppEnv -> IO ()
persistenceApp env = runAppLogIO_ env server
