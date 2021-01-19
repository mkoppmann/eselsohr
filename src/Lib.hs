module Lib
  ( main,
    mkAppEnv,
    runServer,
  )
where

import Lib.App (AppEnv, Env (..), mainLogAction)
import Lib.Config (Config (..), loadConfig)
import qualified Lib.Init as Init
import Lib.Web (application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort, setServerName)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config {..} = do
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

  runSettings settings $ application env

main :: Maybe FilePath -> IO ()
main mConfPath =
  loadConfig mConfPath >>= \conf -> mkAppEnv conf >>= runServer conf
