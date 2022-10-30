module Lib.Infra.Persistence.Server
  ( persistenceApp
  ) where

import qualified Data.Map.Strict                                     as Map

import           UnliftIO.Async                                       ( conc
                                                                      , runConc
                                                                      )
import           UnliftIO.Concurrent                                  ( threadDelay )

import           Lib.App.Env                                          ( envWriteQueue )
import           Lib.App.Port                                         ( MonadTime )
import           Lib.Domain.Repo.CapabilityList                       ( CapabilityListRepo )
import           Lib.Infra.Error                                      ( WithError )
import           Lib.Infra.Log                                        ( pattern E
                                                                      , WithLog
                                                                      , log
                                                                      , runAppLogIO_
                                                                      )
import           Lib.Infra.Monad                                      ( AppEnv )
import           Lib.Infra.Persistence.Cleanup                        ( removeExpiredCapabilitiesFromCollections )
import           Lib.Infra.Persistence.File                           ( WithFile )
import           Lib.Infra.Persistence.Queue                          ( WithQueue
                                                                      , fetchUpdates
                                                                      , processUpdates
                                                                      )

server :: (CapabilityListRepo m, MonadTime m, WithError m, WithFile env m, WithLog env m, WithQueue env m) => m ()
server = do
  commandQueue      <- envWriteQueue
  workerQueueMapVar <- newTVarIO Map.empty
  void $ runJobs commandQueue workerQueueMapVar
  log E "Persistence server finished unexpectedly"
 where
  runJobs commandQueue workerQueueMapVar =
    runConc
      $   (,,)
      <$> conc (fetchUpdates commandQueue workerQueueMapVar)
      <*> (conc (processUpdates workerQueueMapVar) <|> conc cleanup)

cleanup :: (CapabilityListRepo m, MonadTime m, WithError m, WithFile env m) => m Void
cleanup = infinitely $ do
  removeExpiredCapabilitiesFromCollections
  threadDelay $ timeoutInMinutes 60
  where timeoutInMinutes minutes = 1000 * 1000 * 60 * minutes

persistenceApp :: AppEnv -> IO ()
persistenceApp env = runAppLogIO_ env server
