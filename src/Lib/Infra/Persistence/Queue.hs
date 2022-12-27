module Lib.Infra.Persistence.Queue
    ( WithQueue
    , commit
    , fetchUpdates
    , processUpdates
    ) where

import qualified Data.Map.Strict as Map

import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (mapConcurrently_)
import UnliftIO.STM
    ( TQueue
    , isEmptyTQueue
    , newTQueue
    , readTQueue
    , retrySTM
    , tryReadTQueue
    , writeTQueue
    )

import qualified Lib.Domain.Repo as Repo

import Lib.App.Env
    ( HasWriteQueue
    , envWriteQueue
    )
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)
import Lib.Domain.Repo (RepositoryCommandSync)

type WithQueue env m = (MonadReader env m, HasWriteQueue env m, MonadUnliftIO m)

type CommandQueue m = TQueue (RepositoryCommandSync m)
type WorkerQueueMap m = Map (Id Collection) (CommandQueue m)

commit :: (WithQueue env m) => Id Collection -> m () -> m ()
commit colId repoAction = do
    queue <- envWriteQueue
    syncVar <- newEmptyTMVarIO
    let repoCommand = Repo.RepositoryCommandSync syncVar $ Repo.RepositoryCommand colId repoAction
    atomically $ writeTQueue queue repoCommand
    -- Waiting for repo action to finish. This needs to be in a separate atomic
    -- operation, because else we get a live lock.
    atomically $ takeTMVar syncVar

fetchUpdates :: forall env m. (WithQueue env m) => CommandQueue m -> TVar (WorkerQueueMap m) -> m Void
fetchUpdates commandQueue workerQueueMap = infinitely . atomically $ do
    command <- readTQueue commandQueue
    let colId = Repo.collectionId $ Repo.repoCommand command
    addToMap colId command
  where
    addToMap :: Id Collection -> RepositoryCommandSync m -> STM ()
    addToMap colId command = do
        workerQueueMap' <- readTVar workerQueueMap
        case Map.lookup colId workerQueueMap' of
            Just workerQueue -> writeTQueue workerQueue command
            Nothing -> do
                newWorkerQueue <- newTQueue
                writeTQueue newWorkerQueue command
                modifyTVar' workerQueueMap $ Map.insert colId newWorkerQueue

processUpdates :: (WithQueue env m) => TVar (WorkerQueueMap m) -> m Void
processUpdates workerQueueMap = infinitely $ do
    todoQueues <- atomically $ do
        activeList <- filterByActiveQueues workerQueueMap
        if not $ null activeList then pure activeList else retrySTM
    mapConcurrently_ worker todoQueues
    atomically cleanupMap
  where
    cleanupMap :: STM ()
    cleanupMap = writeTVar workerQueueMap =<< filterByActiveQueues workerQueueMap

    filterByActiveQueues :: TVar (WorkerQueueMap m) -> STM (WorkerQueueMap m)
    filterByActiveQueues =
        readTVar >=> Map.toList >>> filterM (pure . not <=< isEmptyTQueue . snd) >=> Map.fromList >>> pure

worker :: (WithQueue env m) => CommandQueue m -> m ()
worker queue =
    join . atomically $
        tryReadTQueue queue >>= \case
            Nothing -> pure pass
            Just update -> pure $ storeUpdate update >> worker queue

storeUpdate :: (WithQueue env m) => RepositoryCommandSync m -> m ()
storeUpdate Repo.RepositoryCommandSync{..} = do
    Repo.action repoCommand
    atomically (putTMVar syncVar ())
