{- | Configuration options for Eselsohr.
Can be configured via environment variables or config file.
-}

module Config
  ( Config(..)
  , loadConfig
  ) where

import           Colog                                                ( Severity(Error) )
import           Configuration.Dotenv                                 ( configPath
                                                                      , defaultConfig
                                                                      , loadFile
                                                                      , onMissingFile
                                                                      )
import           Control.Monad.Catch                                  ( MonadCatch )
import           Data.Text                                            ( toTitle )
import           Network.Wai.Handler.Warp                             ( Port )
import           System.FilePath                                      ( addTrailingPathSeparator )
import           UnliftIO.Directory                                   ( XdgDirectory(XdgData)
                                                                      , canonicalizePath
                                                                      , getXdgDirectory
                                                                      )
import           UnliftIO.Environment                                 ( getEnvironment
                                                                      , unsetEnv
                                                                      )

import qualified Lib.App.Env                                         as Env

import           Lib.App.Env                                          ( DataPath
                                                                      , DeploymentMode
                                                                      )

{- | All configuration options.
-}
data Config = Config
  { -- | File path to the data folder, where all collection are getting stored.
    -- Defaults to: 'XdgData'
    confDataFolder               :: !DataPath
  , -- | Severity level for the logger component.
    -- Defaults to: 'Error'
    confLogSeverity              :: !Severity
  , -- | Port number on which the web server will listen.
    -- Defaults to: @6979@
    confServerPort               :: !Port
  , -- | Address where the web server will listen.
    -- Defaults to: @127.0.0.1@
    confListenAddr               :: !String
  , -- | Send @HSTS@ HTTP header.
    -- Automatically enabled when @X-Forwarded-Proto@ HTTP header is set to
    -- @https@.
    -- Defaults to: 'False'
    confHttps                    :: !Bool
  , -- | Do not send @HSTS@ HTTP header, when @HTTPS@ is set.
    -- Defaults to: 'False'
    confDisableHsts              :: !Bool
  , -- | File path to the TLS certificate file.
    -- Defaults to: 'certificate.pem'
    confCertFile                 :: !FilePath
  , -- | File path to the TLS key file.
    -- Defaults to: 'key.pem'
    confKeyFile                  :: !FilePath
  , -- | The mode the application is running in.
    -- Defaults to: 'Prod'
    confDepMode                  :: !DeploymentMode
  , -- | Wether the creation of collections should be public.
    -- Defaults to: 'False'
    confPublicCollectionCreation :: !Bool
  }

{- | Load Eselsohr configuration via environmental variables.
Accepts an optional 'FilePath' argument for a configuration file. By default
this function will look for a @.env@ file in the current working directory.
Variables that are set via real env vars have the highest priority.
-}
loadConfig :: (MonadCatch m, MonadIO m) => Maybe FilePath -> m Config
loadConfig mConfPath = do
  loadEnvFile mConfPath
  confDataFolder               <- getDataFolder
  confLogSeverity              <- getLogLevel
  confServerPort               <- getPort
  confListenAddr               <- getListenAddr
  confHttps                    <- getHttps
  confDisableHsts              <- getDisableHsts
  confCertFile                 <- getCertFile
  confKeyFile                  <- getKeyFile
  confDepMode                  <- getDeploymentMode
  confPublicCollectionCreation <- getCollectionCreation
  clearEnv
  pure $ Config { .. }
 where
  loadEnvFile :: (MonadCatch m, MonadIO m) => Maybe FilePath -> m ()
  loadEnvFile Nothing   = onMissingFile (void $ loadFile defaultConfig) pass
  loadEnvFile (Just fp) = void . loadFile $ defaultConfig { configPath = [fp] }

  getDataFolder :: (MonadIO m) => m DataPath
  getDataFolder = lookupEnv "DATA_FOLDER" >>= \case
    Nothing -> getXdgDirectory XdgData "eselsohr" >>= sanitizePath
    Just df -> sanitizePath df

  getLogLevel :: (MonadIO m) => m Severity
  getLogLevel = lookupEnv "LOG_LEVEL" >>= \case
    Nothing -> pure Error
    Just ls -> pure . fromMaybe Error . readMaybe $ toTitleCase ls

  getPort :: (MonadIO m) => m Port
  getPort = lookupEnv "PORT" >>= \case
    Nothing -> pure 6979
    Just sp -> pure . fromMaybe 6979 $ isInPortRange =<< readMaybe sp

  getListenAddr :: (MonadIO m) => m String
  getListenAddr = lookupEnv "LISTEN_ADDR" >>= \case
    Nothing -> pure "127.0.0.1"
    Just la -> pure la

  getHttps :: (MonadIO m) => m Bool
  getHttps = lookupEnv "HTTPS" >>= \case
    Nothing -> pure False
    Just hs -> pure . fromMaybe False . readMaybe $ toTitleCase hs

  getDisableHsts :: (MonadIO m) => m Bool
  getDisableHsts = lookupEnv "DISABLE_HSTS" >>= \case
    Nothing -> pure False
    Just dh -> pure . fromMaybe False . readMaybe $ toTitleCase dh

  getCertFile :: (MonadIO m) => m FilePath
  getCertFile = maybe (pure "certificate.pem") sanitizePath =<< lookupEnv "CERT_FILE"

  getKeyFile :: (MonadIO m) => m FilePath
  getKeyFile = maybe (pure "key.pem") sanitizePath =<< lookupEnv "KEY_FILE"

  getDeploymentMode :: (MonadIO m) => m DeploymentMode
  getDeploymentMode = lookupEnv "DEPLOYMENT_MODE" >>= \case
    Nothing -> pure Env.Prod
    Just e  -> pure . fromMaybe Env.Prod . readMaybe $ toTitleCase e

  getCollectionCreation :: (MonadIO m) => m Bool
  getCollectionCreation = lookupEnv "PUBLIC_COLLECTION_CREATION" >>= \case
    Nothing  -> pure False
    Just pcc -> pure . fromMaybe False . readMaybe $ toTitleCase pcc

clearEnv :: (MonadIO m) => m ()
clearEnv = traverse_ (unsetEnv . fst) =<< getEnvironment

sanitizePath :: (MonadIO m) => FilePath -> m FilePath
sanitizePath p = addTrailingPathSeparator <$> canonicalizePath p

isInPortRange :: Int -> Maybe Port
isInPortRange p | p >= 0 && p <= 65535 = Just p
                | otherwise            = Nothing

toTitleCase :: String -> String
toTitleCase = toString . toTitle . toText
