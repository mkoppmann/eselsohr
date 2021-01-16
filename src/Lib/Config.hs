module Lib.Config
  ( Config (..),
    loadConfig,
  )
where

import Colog (Severity (Error))
import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Text (toTitle)
import Network.Wai.Handler.Warp (Port)
import System.FilePath (addTrailingPathSeparator)
import UnliftIO.Directory (XdgDirectory (XdgData), getXdgDirectory)
import UnliftIO.Environment (lookupEnv)

data Config = Config
  { confDataFolder :: !FilePath,
    confLogSeverity :: !Severity,
    confServerPort :: !Port,
    confListenAddr :: !String
  }

loadConfig :: (MonadIO m) => m Config
loadConfig = do
  void $ loadFile defaultConfig
  df <- getDataFolder
  sp <- getPort
  la <- getListenAddr
  ls <- getLogLevel
  pure $ Config df ls sp la
  where
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
