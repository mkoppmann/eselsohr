module Config
    ( Config (..)
    , loadConfig
    ) where

import Colog (Severity (Error))
import Configuration.Dotenv
    ( configPath
    , defaultConfig
    , loadFile
    , onMissingFile
    )
import Control.Monad.Catch (MonadCatch)
import Data.Text (toTitle)
import Network.Wai.Handler.Warp (Port)
import System.FilePath (addTrailingPathSeparator)
import UnliftIO.Directory
    ( XdgDirectory (XdgData)
    , canonicalizePath
    , getXdgDirectory
    )
import UnliftIO.Environment
    ( getEnvironment
    , unsetEnv
    )

import qualified Lib.App.Env as Env

import Lib.App.Env
    ( DataPath
    , DeploymentMode
    )
import Lib.Domain.Uri
    ( Uri
    , baseUri
    )

{- | Configuration options for Eselsohr.
 Can be configured via environment variables or config file.
-}
data Config = Config
    { confDataFolder :: !DataPath
    -- ^ File path to the data folder, where all collection are getting stored.
    -- Defaults to: 'XdgData'
    , confLogSeverity :: !Severity
    -- ^ Severity level for the logger component.
    -- Defaults to: 'Error'
    , confServerPort :: !Port
    -- ^ Port number on which the web server will listen.
    -- Defaults to: @6979@
    , confBaseUrl :: !Uri
    -- ^ Base URL to generate HTML links.
    -- Defaults to @http://localhost@
    , confListenAddr :: !String
    -- ^ Address where the web server will listen.
    -- Defaults to: @127.0.0.1@
    , confHttps :: !Bool
    -- ^ Send @HSTS@ HTTP header.
    -- Automatically enabled when @X-Forwarded-Proto@ HTTP header is set to
    -- @https@.
    -- Defaults to: 'False'
    , confDisableHsts :: !Bool
    -- ^ Do not send @HSTS@ HTTP header, when @HTTPS@ is set.
    -- Defaults to: 'False'
    , confCertFile :: !FilePath
    -- ^ File path to the TLS certificate file.
    -- Defaults to: 'certificate.pem'
    , confKeyFile :: !FilePath
    -- ^ File path to the TLS key file.
    -- Defaults to: 'key.pem'
    , confDepMode :: !DeploymentMode
    -- ^ The mode the application is running in.
    -- Defaults to: 'Prod'
    , confPublicCollectionCreation :: !Bool
    -- ^ Wether the creation of collections should be public.
    -- Defaults to: 'False'
    , confStaticFolderPath :: !FilePath
    -- ^ The path to the folder with static resources.
    -- Defaults to: 'static/'
    }

loadConfig :: (MonadCatch m, MonadIO m) => Maybe FilePath -> m Config
loadConfig mConfPath = do
    loadEnvFile mConfPath
    confDataFolder <- getDataFolder
    confLogSeverity <- getLogLevel
    confServerPort <- getPort
    confBaseUrl <- getBaseUrl confServerPort
    confListenAddr <- getListenAddr
    confHttps <- getHttps
    confDisableHsts <- getDisableHsts
    confCertFile <- getCertFile
    confKeyFile <- getKeyFile
    confDepMode <- getDeploymentMode
    confPublicCollectionCreation <- getCollectionCreation
    confStaticFolderPath <- getStaticFolderPath
    clearEnv
    pure $ Config{..}
  where
    loadEnvFile :: (MonadCatch m, MonadIO m) => Maybe FilePath -> m ()
    loadEnvFile Nothing = onMissingFile (void $ loadFile defaultConfig) pass
    loadEnvFile (Just fp) = void . loadFile $ defaultConfig{configPath = [fp]}

    getDataFolder :: (MonadIO m) => m DataPath
    getDataFolder =
        lookupEnv "DATA_FOLDER" >>= \case
            Nothing -> getXdgDirectory XdgData "eselsohr" >>= sanitizePath
            Just df -> sanitizePath df

    getLogLevel :: (MonadIO m) => m Severity
    getLogLevel =
        lookupEnv "LOG_LEVEL" >>= \case
            Nothing -> pure Error
            Just ls -> pure . fromMaybe Error . readMaybe $ toTitleCase ls

    getPort :: (MonadIO m) => m Port
    getPort =
        lookupEnv "PORT" >>= \case
            Nothing -> pure 6979
            Just sp -> pure . fromMaybe 6979 $ isInPortRange =<< readMaybe sp

    getBaseUrl :: (MonadIO m) => Port -> m Uri
    getBaseUrl port =
        lookupEnv "BASE_URL" >>= \case
            Nothing -> pure . baseUri $ "http://localhost:" <> show port <> "/"
            Just bu -> pure . baseUri $ toText bu

    getListenAddr :: (MonadIO m) => m String
    getListenAddr =
        lookupEnv "LISTEN_ADDR" >>= \case
            Nothing -> pure "127.0.0.1"
            Just la -> pure la

    getHttps :: (MonadIO m) => m Bool
    getHttps =
        lookupEnv "HTTPS" >>= \case
            Nothing -> pure False
            Just hs -> pure . fromMaybe False . readMaybe $ toTitleCase hs

    getDisableHsts :: (MonadIO m) => m Bool
    getDisableHsts =
        lookupEnv "DISABLE_HSTS" >>= \case
            Nothing -> pure False
            Just dh -> pure . fromMaybe False . readMaybe $ toTitleCase dh

    getCertFile :: (MonadIO m) => m FilePath
    getCertFile = maybe (pure "certificate.pem") sanitizePath =<< lookupEnv "CERT_FILE"

    getKeyFile :: (MonadIO m) => m FilePath
    getKeyFile = maybe (pure "key.pem") sanitizePath =<< lookupEnv "KEY_FILE"

    getDeploymentMode :: (MonadIO m) => m DeploymentMode
    getDeploymentMode =
        lookupEnv "DEPLOYMENT_MODE" >>= \case
            Nothing -> pure Env.Prod
            Just e -> pure . fromMaybe Env.Prod . readMaybe $ toTitleCase e

    getCollectionCreation :: (MonadIO m) => m Bool
    getCollectionCreation =
        lookupEnv "PUBLIC_COLLECTION_CREATION" >>= \case
            Nothing -> pure False
            Just pcc -> pure . fromMaybe False . readMaybe $ toTitleCase pcc

    getStaticFolderPath :: (MonadIO m) => m FilePath
    getStaticFolderPath = maybe (pure "static/") sanitizePath =<< lookupEnv "STATIC_FOLDER_PATH"

clearEnv :: (MonadIO m) => m ()
clearEnv = traverse_ (unsetEnv . fst) =<< getEnvironment

sanitizePath :: (MonadIO m) => FilePath -> m FilePath
sanitizePath p = addTrailingPathSeparator <$> canonicalizePath p

isInPortRange :: Int -> Maybe Port
isInPortRange p
    | p >= 0 && p <= 65535 = Just p
    | otherwise = Nothing

toTitleCase :: String -> String
toTitleCase = toString . toTitle . toText
