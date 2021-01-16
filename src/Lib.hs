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
import Network.Wai.Handler.Warp (run)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config {..} = do
  let envDataFolder = confDataFolder
  let envLogAction = mainLogAction confLogSeverity
  let envServerPort = confServerPort
  return Env {..}

runServer :: AppEnv -> IO ()
runServer env@Env {..} = do
  Init.datafolder envDataFolder
  Init.initialCap envDataFolder
  print @Text "Eselsohr is now running."
  print @Text $ "Access it on: http://localhost:" <> show envServerPort
  run envServerPort $ application env

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
