module Lib
  ( main,
    mkAppEnv,
    runServer,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Lib.App (AppEnv, Env (..), mainLogAction)
import Lib.Config (Config (..), loadConfig)
import qualified Lib.Init as Init
import Lib.Web (application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort, setServerName)
import qualified System.Metrics as Metrics
import System.Remote.Monitoring (forkServerWith)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config {..} = do
  envTimings <- newIORef HashMap.empty
  envEkgStore <- Metrics.newStore

  let envDataFolder = confDataFolder
  let envLogAction = mainLogAction confLogSeverity
  let envBaseUrl = confBaseUrl

  return Env {..}

runServer :: Config -> AppEnv -> IO ()
runServer Config {..} env@Env {..} = do
  Init.datafolder envDataFolder
  Init.initialCap envDataFolder

  let settings =
        setHost (fromString confListenAddr)
          . setPort confServerPort
          . setServerName ""
          $ defaultSettings

  print @Text "Eselsohr is now running."
  print @Text $
    "Access it on: http://"
      <> toText confListenAddr
      <> ":"
      <> show confServerPort

  Metrics.registerGcMetrics envEkgStore
  () <$ forkServerWith envEkgStore "localhost" 6980
  runSettings settings $ application env

main :: IO ()
main = loadConfig >>= \conf -> mkAppEnv conf >>= runServer conf
