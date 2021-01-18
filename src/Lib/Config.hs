module Lib.Config
  ( Config (..),
    loadConfig,
  )
where

import Colog (Severity (Error))
import Configuration.Dotenv (configPath, defaultConfig, loadFile, onMissingFile)
import Control.Monad.Catch (MonadCatch)
import Data.Maybe (fromJust)
import Data.Text (toTitle)
import Lib.Core.Domain.Uri (Uri (..))
import Network.Wai.Handler.Warp (Port)
import System.FilePath (addTrailingPathSeparator)
import qualified Text.URI as U
import UnliftIO.Directory (XdgDirectory (XdgData), getXdgDirectory)
import UnliftIO.Environment (lookupEnv)

data Config = Config
  { confDataFolder :: !FilePath,
    confLogSeverity :: !Severity,
    confServerPort :: !Port,
    confListenAddr :: !String,
    confBaseUrl :: !Uri
  }

loadConfig :: (MonadCatch m, MonadIO m) => Maybe FilePath -> m Config
loadConfig mConfPath = do
  loadEnvFile mConfPath
  df <- getDataFolder
  sp <- getPort
  la <- getListenAddr
  bu <- getBaseUrl sp
  ls <- getLogLevel
  pure $ Config df ls sp la bu
  where
    loadEnvFile :: (MonadCatch m, MonadIO m) => Maybe FilePath -> m ()
    loadEnvFile = \case
      Nothing -> onMissingFile (void $ loadFile defaultConfig) (pure ())
      Just fp -> do
        let config = defaultConfig {configPath = [fp]}
        void $ loadFile config

    getDataFolder :: (MonadIO m) => m String
    getDataFolder = do
      mDF <- lookupEnv "DATA_FOLDER"
      case mDF of
        Nothing -> getXdgDirectory XdgData $ addTrailingPathSeparator "eselsohr"
        Just df -> pure $ addTrailingPathSeparator df

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
      case mBU of
        Nothing ->
          pure . Uri . fromJust . U.mkURI $ "http://localhost:" <> show port <> "/"
        Just bu ->
          case U.mkURI $ toText bu of
            Left err ->
              error . toText $ "Invalid base url: " <> displayException err
            Right uri -> pure $ Uri uri
