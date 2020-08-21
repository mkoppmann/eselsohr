module Lib
  ( main,
    mkAppEnv,
    runServer,
  )
where

import Lib.App (AppEnv, Env (..))
import Lib.Config (Config (..), loadConfig)
import Lib.Effect.Log (mainLogAction)
import Lib.Web (application)
import Network.Wai.Handler.Warp (run)
import System.Directory (createDirectoryIfMissing)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config {..} = do
  let envDataFolder = confDataFolder
  let envLogAction = mainLogAction confLogSeverity
  let envServerPort = confServerPort
  return Env {..}

prepareDatafolder :: FilePath -> IO ()
prepareDatafolder = createDirectoryIfMissing True

runServer :: AppEnv -> IO ()
runServer env@Env {..} = do
  prepareDatafolder envDataFolder
  run envServerPort $ application env

main :: IO ()
main = mkAppEnv loadConfig >>= runServer
