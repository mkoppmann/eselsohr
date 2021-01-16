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
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config {..} = do
  let envDataFolder = confDataFolder
  let envLogAction = mainLogAction confLogSeverity
  return Env {..}

runServer :: Config -> AppEnv -> IO ()
runServer Config {..} env@Env {..} = do
  Init.datafolder envDataFolder
  Init.initialCap envDataFolder

  let settings =
        setHost (fromString confListenAddr)
          . setPort confServerPort
          $ defaultSettings

  print @Text "Eselsohr is now running."
  print @Text $
    "Access it on: http://"
      <> toText confListenAddr
      <> ":"
      <> show confServerPort

  runSettings settings $ application env

main :: IO ()
main = loadConfig >>= \conf -> mkAppEnv conf >>= runServer conf
