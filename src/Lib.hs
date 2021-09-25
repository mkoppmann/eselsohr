module Lib
  ( main
  , mkAppEnv
  , runServer
  ) where

import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                , Hsts(..)
                                                , Https(..)
                                                , mainLogAction
                                                )
import           Lib.Config                     ( Config(..)
                                                , loadConfig
                                                )
import qualified Lib.Init                      as Init
import           Lib.Persistence                ( persistenceApp )
import           Lib.Web                        ( application )
import           Network.TLS                    ( Version(..) )
import           Network.TLS.Cipher             ( Cipher )
import qualified Network.TLS.Extra.Cipher      as TLS
import           Network.Wai.Handler.Warp       ( Settings
                                                , defaultSettings
                                                , runSettings
                                                , setHost
                                                , setPort
                                                , setServerName
                                                )
import           Network.Wai.Handler.WarpTLS    ( OnInsecure(..)
                                                , TLSSettings(..)
                                                , runTLS
                                                , tlsSettings
                                                )
import           UnliftIO.Async                 ( race_ )
import           UnliftIO.STM                   ( newTQueueIO )

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config {..} = do
  writeQueue <- newTQueueIO

  let envDataFolder          = confDataFolder
      envMaxConcurrentWrites = confMaxConcurrentWrites
      envWriteQueue          = writeQueue
      envLogAction           = mainLogAction confLogSeverity
      envBaseUrl             = confBaseUrl
      envHttps               = if confHttps then HttpsOn else HttpsOff
      envHsts                = if confDisableHsts then HstsOff else HstsOn
  pure Env { .. }

runServer :: Config -> AppEnv -> IO ()
runServer Config {..} env@Env {..} = do
  Init.datafolder envDataFolder
  Init.useRestrictedHttpManager

  let settings =
        setHost (fromString confListenAddr)
          . setPort confServerPort
          . setServerName ""
          $ defaultSettings

  case envHttps of
    HttpsOn  -> startTlsServer confCertFile confKeyFile settings
    HttpsOff -> startServer settings
 where
  startTlsServer :: FilePath -> FilePath -> Settings -> IO ()
  startTlsServer certFile keyFile settings = do
    let tlsOpts  = tlsSettings certFile keyFile
        tlsOpts' = tlsOpts { onInsecure         = AllowInsecure
                           , tlsAllowedVersions = [TLS13, TLS12]
                           , tlsCiphers         = tlsCiphers
                           }
    printIntro
    race_ (persistenceApp env)
          (runTLS tlsOpts' settings $ application confServerPort env)
    print @Text "Program ended"

  startServer :: Settings -> IO ()
  startServer settings = do
    printIntro
    race_ (persistenceApp env)
          (runSettings settings $ application confServerPort env)
    print @Text "Program ended"

  printIntro :: IO ()
  printIntro = do
    print @Text "Eselsohr is now running."
    print
      $  protocolText envHttps
      <> toText confListenAddr
      <> ":"
      <> show confServerPort

  protocolText :: Https -> Text
  protocolText HttpsOn  = "Access it on: https://"
  protocolText HttpsOff = "Access it on: http://"

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

main :: Maybe FilePath -> IO ()
main mConfPath = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  loadConfig mConfPath >>= \conf -> mkAppEnv conf >>= runServer conf
