module Lib.Config
  ( Config (..),
    loadConfig,
  )
where

import Colog (Severity (Error))
import Configuration.Dotenv (configPath, defaultConfig, loadFile, onMissingFile)
import Control.Monad.Catch (MonadCatch)
import Data.Text (toTitle)
import Lib.App.Env (DataPath, MaxConcurrentWrites)
import Lib.Core.Domain.Uri (Uri (..), baseUri)
import Network.Wai.Handler.Warp (Port)
import System.FilePath (addTrailingPathSeparator)
import UnliftIO.Directory (XdgDirectory (XdgData), getXdgDirectory)

-- | Configuration options for Eselsohr.
-- Can be configured via environment variables or config file.
data Config = Config
  { -- | File path to the data folder, where all collection are getting stored.
    -- Defaults to: 'XdgData'
    confDataFolder :: !DataPath,
    -- | Number of max concurrently run write operations.
    -- Set a limit to avoid resource exhaustion.
    -- Must be larger or equal than 1.
    -- Defaults to: 'Nothing'
    confMaxConcurrentWrites :: !(Maybe MaxConcurrentWrites),
    -- | Severity level for the logger component.
    -- Defaults to: 'Error'
    confLogSeverity :: !Severity,
    -- | Port number on which the web server will listen.
    -- Defaults to: @6979@
    confServerPort :: !Port,
    -- | Address where the web server will listen.
    -- Defaults to: @127.0.0.1@
    confListenAddr :: !String,
    -- | Base URL to generate HTML links.
    -- Defaults to @http://localhost@
    confBaseUrl :: !Uri,
    -- | Send @HSTS@ HTTP header.
    -- Automatically enabled when @X-Forwarded-Proto@ HTTP header is set to
    -- @https@.
    -- Defaults to: 'False'
    confHttps :: !Bool,
    -- | Do not send @HSTS@ HTTP header, when @HTTPS@ is set.
    -- Defaults to: 'False'
    confDisableHsts :: !Bool,
    -- | File path to the TLS certificate file.
    -- Defaults to: 'certificate.pem'
    confCertFile :: !FilePath,
    -- | File path to the TLS key file.
    -- Defaults to: 'key.pem'
    confKeyFile :: !FilePath
  }

loadConfig :: (MonadCatch m, MonadIO m) => Maybe FilePath -> m Config
loadConfig mConfPath = do
  loadEnvFile mConfPath
  df <- getDataFolder
  mw <- getMaxConcurrentWrites
  sp <- getPort
  la <- getListenAddr
  bu <- getBaseUrl sp
  hs <- getHttps
  dh <- getDisableHsts
  cf <- getCertFile
  kf <- getKeyFile
  ls <- getLogLevel
  pure $ Config df mw ls sp la bu hs dh cf kf
  where
    loadEnvFile :: (MonadCatch m, MonadIO m) => Maybe FilePath -> m ()
    loadEnvFile = \case
      Nothing -> onMissingFile (void $ loadFile defaultConfig) pass
      Just fp -> do
        let config = defaultConfig {configPath = [fp]}
        void $ loadFile config

    getDataFolder :: (MonadIO m) => m DataPath
    getDataFolder = do
      mDF <- lookupEnv "DATA_FOLDER"
      case mDF of
        Nothing -> getXdgDirectory XdgData $ addTrailingPathSeparator "eselsohr"
        Just df -> pure $ addTrailingPathSeparator df

    getMaxConcurrentWrites :: (MonadIO m) => m (Maybe MaxConcurrentWrites)
    getMaxConcurrentWrites = do
      mMW <- lookupEnv "MAX_CONCURRENT_WRITES"
      case mMW of
        Nothing -> pure Nothing
        Just mw -> pure $ case readMaybe mw of
          Nothing -> Nothing
          Just maxWrites -> if maxWrites > 0 then Just maxWrites else Nothing

    getLogLevel :: (MonadIO m) => m Severity
    getLogLevel = do
      mLS <- lookupEnv "LOG_LEVEL"
      case mLS of
        Nothing -> pure Error
        Just ls ->
          pure . fromMaybe Error . readMaybe . toString . toTitle $ toText ls

    getPort :: (MonadIO m) => m Port
    getPort = do
      mSP <- lookupEnv "PORT"
      case mSP of
        Nothing -> pure 6979
        Just sp -> pure . fromMaybe 6979 $ readMaybe sp

    getListenAddr :: (MonadIO m) => m String
    getListenAddr = do
      mLA <- lookupEnv "LISTEN_ADDR"
      case mLA of
        Nothing -> pure "127.0.0.1"
        Just la -> pure la

    getBaseUrl :: (MonadIO m) => Port -> m Uri
    getBaseUrl port = do
      mBU <- lookupEnv "BASE_URL"
      let url = case mBU of
            Nothing -> "http://localhost:" <> show port <> "/"
            Just bu -> toText bu
      pure $ baseUri url

    getHttps :: (MonadIO m) => m Bool
    getHttps = do
      mHS <- lookupEnv "HTTPS"
      case mHS of
        Nothing -> pure False
        Just hs ->
          pure . fromMaybe False . readMaybe . toString . toTitle $ toText hs

    getDisableHsts :: (MonadIO m) => m Bool
    getDisableHsts = do
      mDH <- lookupEnv "DISABLE_HSTS"
      case mDH of
        Nothing -> pure False
        Just dh ->
          pure . fromMaybe False . readMaybe . toString . toTitle $ toText dh

    getCertFile :: (MonadIO m) => m FilePath
    getCertFile = maybe (pure "certificate.pem") pure =<< lookupEnv "CERT_FILE"

    getKeyFile :: (MonadIO m) => m FilePath
    getKeyFile = maybe (pure "key.pem") pure =<< lookupEnv "KEY_FILE"
