module Lib.Infra.Persistence.Server
  ( persistenceApp
  ) where

import qualified Data.Map.Strict                                     as Map

import           UnliftIO.Async                                       ( race_ )

import           Lib.App.Env                                          ( MaxConcurrentWrites
                                                                      , envWriteQueue
                                                                      , grab
                                                                      )
import           Lib.Infra.Log                                        ( pattern E
                                                                      , WithLog
                                                                      , log
                                                                      , runAppLogIO_
                                                                      )
import           Lib.Infra.Monad                                      ( AppEnv )
import           Lib.Infra.Persistence.Queue                          ( WithQueue
                                                                      , fetchUpdates
                                                                      , processUpdates
                                                                      )

server :: (WithLog env m, WithQueue env m) => m ()
server = do
  commandQueue         <- envWriteQueue
  mMaxConcurrentWrites <- grab @(Maybe MaxConcurrentWrites)
  workerQueueMapVar    <- newTVarIO Map.empty
  race_ (fetchUpdates commandQueue workerQueueMapVar) (processUpdates workerQueueMapVar mMaxConcurrentWrites)
  log E "Persistence server finished unexpectedly"

persistenceApp :: AppEnv -> IO ()
persistenceApp env = runAppLogIO_ env server
