module Lib
  ( main
  , mkAppEnv
  , warpSettings
  , runServer
  ) where

import qualified Network.TLS.Extra.Cipher                            as TLS

import           Network.TLS                                          ( Version(..) )
import           Network.TLS.Cipher                                   ( Cipher )
import           Network.Wai.Handler.Warp                             ( Port
                                                                      , Settings
                                                                      , defaultSettings
                                                                      , runSettings
                                                                      , setHost
                                                                      , setPort
                                                                      , setServerName
                                                                      )
import           Network.Wai.Handler.WarpTLS                          ( OnInsecure(..)
                                                                      , TLSSettings(..)
                                                                      , runTLS
                                                                      , tlsSettings
                                                                      )
import           UnliftIO.Async                                       ( race_ )
import           UnliftIO.STM                                         ( newTQueueIO )

import qualified Config
import qualified Init
import qualified Lib.App.Env                                         as Env
import qualified Lib.Ui.Cli.Handler                                  as Cli
import qualified Migration

import           Config                                               ( Config
                                                                      , loadConfig
                                                                      )
import           Lib.App.Env                                          ( Https )
import           Lib.Infra.Log                                        ( mainLogAction )
import           Lib.Infra.Monad                                      ( AppEnv )
import           Lib.Infra.Persistence.Server                         ( persistenceApp )
import           Lib.Ui.Cli.Handler                                   ( CliAction )
import           Lib.Ui.Server                                        ( application )

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config.Config {..} = do
  newWriteQueue <- newTQueueIO
  let dataFolder         = confDataFolder
      baseUrl            = confBaseUrl
      writeQueue         = newWriteQueue
      logAction          = mainLogAction confLogSeverity
      https              = if confHttps then Env.HttpsOn else Env.HttpsOff
      hsts               = if confDisableHsts then Env.HstsOff else Env.HstsOn
      deploymentMode     = confDepMode
      collectionCreation = if confPublicCollectionCreation then Env.Public else Env.Private
  pure $ Env.Env { .. }

runServer :: Config -> AppEnv -> IO ()
runServer Config.Config {..} env@Env.Env {..} = do
  Init.datafolder dataFolder
  Init.useRestrictedHttpManager
  printIntro
  case https of
    Env.HttpsOn  -> startTlsServer confCertFile confKeyFile
    Env.HttpsOff -> startServer
 where
  warpSettings' :: Settings
  warpSettings' = warpSettings confListenAddr confServerPort

  startTlsServer :: FilePath -> FilePath -> IO ()
  startTlsServer certFile keyFile = do
    let tlsOpts  = tlsSettings certFile keyFile
        tlsOpts' = tlsOpts { onInsecure = AllowInsecure, tlsAllowedVersions = [TLS13, TLS12], tlsCiphers = tlsCiphers }
    race_ runPersistence (runTLS tlsOpts' warpSettings' $ application confServerPort env)
    print @Text "Program ended"

  startServer :: IO ()
  startServer = do
    race_ runPersistence (runSettings warpSettings' $ application confServerPort env)
    print @Text "Program ended"

  runPersistence :: IO ()
  runPersistence = persistenceApp env

  printIntro :: IO ()
  printIntro = do
    print @Text "Eselsohr is now running."
    print $ protocolText https <> toText confListenAddr <> ":" <> show confServerPort

  protocolText :: Https -> Text
  protocolText Env.HttpsOn  = "Access it on: https://"
  protocolText Env.HttpsOff = "Access it on: http://"

  {- | Based on the Mozilla Intermediate TLS configuration
    https://wiki.mozilla.org/Security/Server_Side_TLS#Intermediate_compatibility_.28recommended.29
  -}
  tlsCiphers :: [Cipher]
  tlsCiphers =
    [ TLS.cipher_TLS13_AES128GCM_SHA256
    , TLS.cipher_TLS13_AES256GCM_SHA384
    , TLS.cipher_TLS13_CHACHA20POLY1305_SHA256
    , TLS.cipher_ECDHE_ECDSA_AES128GCM_SHA256
    , TLS.cipher_ECDHE_RSA_AES128GCM_SHA256
    , TLS.cipher_ECDHE_ECDSA_AES256GCM_SHA384
    , TLS.cipher_ECDHE_RSA_AES256GCM_SHA384
    , TLS.cipher_ECDHE_ECDSA_CHACHA20POLY1305_SHA256
    , TLS.cipher_ECDHE_RSA_CHACHA20POLY1305_SHA256
    , TLS.cipher_DHE_RSA_AES128GCM_SHA256
    , TLS.cipher_DHE_RSA_AES256GCM_SHA384
    ]

warpSettings :: String -> Port -> Settings
warpSettings listenAddr port = setHost (fromString listenAddr) . setPort port . setServerName "" $ defaultSettings

main :: Maybe FilePath -> CliAction -> IO ()
main mConfPath command = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  conf <- loadConfig mConfPath
  env  <- mkAppEnv conf
  case command of
    Cli.RunServer    -> runServer conf env
    Cli.Migrate      -> Migration.migrate $ Config.confDataFolder conf
    _otherCliCommand -> Cli.runCli env command
