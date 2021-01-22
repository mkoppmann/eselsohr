module Lib
  ( main,
    mkAppEnv,
    runServer,
  )
where

import Lib.App (AppEnv, Env (..), Hsts (..), Https (..), mainLogAction)
import Lib.Config (Config (..), loadConfig)
import qualified Lib.Init as Init
import Lib.Web (application)
import Network.TLS (Version (..))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort, setServerName)
import Network.Wai.Handler.WarpTLS (OnInsecure (..), TLSSettings (..), runTLS, tlsSettings)

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config {..} = do
  let envDataFolder = confDataFolder
  let envLogAction = mainLogAction confLogSeverity
  let envBaseUrl = confBaseUrl
  let envHttps = if confHttps then HttpsOn else HttpsOff
  let envHsts = if confDisableHsts then HstsOff else HstsOn
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

  case envHttps of
    HttpsOn -> startTlsServer confCertFile confKeyFile settings
    HttpsOff -> startServer settings
  where
    startTlsServer certFile keyFile settings = do
      let tlsOpts = tlsSettings certFile keyFile
      let tlsOpts' =
            tlsOpts
              { onInsecure = AllowInsecure,
                tlsAllowedVersions = [TLS13, TLS12]
              }
      printIntro
      runTLS tlsOpts' settings $ application confServerPort env

    startServer settings = do
      printIntro
      runSettings settings $ application confServerPort env

    printIntro = do
      print @Text "Eselsohr is now running."
      print @Text $
        protocolText envHttps
          <> toText confListenAddr
          <> ":"
          <> show confServerPort

    protocolText HttpsOn = "Access it on: https://"
    protocolText HttpsOff = "Access it on: http://"

main :: Maybe FilePath -> IO ()
main mConfPath =
  loadConfig mConfPath >>= \conf -> mkAppEnv conf >>= runServer conf
