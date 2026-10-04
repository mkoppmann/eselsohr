{- |
Module      : Main
Description : Command entry point

@--version@ prints the package name and version.
@serve@ loads the configuration from the environment, resolves the collections directory, and runs the web server until the process ends.
@create-collection@ loads the same configuration, creates a collection in the collections directory, and prints its owner URL once on standard output, formed as the public origin, @/b/@, and the owner bearer.
A configuration error, an unusable collections directory, or a listener that cannot be resolved or bound is reported on standard error with a failing exit status before any request is served or file created.
Other arguments produce a usage message on standard error and a failing exit status.
-}
module Main (main) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Eselsohr.Config (Config (..), PublicOrigin, collectionsDirectory, loadConfig, renderConfigError, renderPublicOrigin)
import Eselsohr.Store.Runtime (resolveDirectory)
import Eselsohr.Store.SQLite.Bearer (Bearer, renderBearer)
import Eselsohr.Store.SQLite.Create (CreateError (..), createCollection)
import Eselsohr.Version (version)
import Eselsohr.Web.Server qualified as Server
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn ("eselsohr-v2 " ++ version)
        ["serve"] -> withConfig serve
        ["create-collection"] -> withConfig createOwnerUrl
        _ -> do
            hPutStrLn stderr "Usage: eselsohr-v2 serve | eselsohr-v2 create-collection | eselsohr-v2 --version"
            exitFailure

withConfig :: (Config -> IO ()) -> IO ()
withConfig run = do
    loaded <- loadConfig lookupEnv
    case loaded of
        Left err -> failWith (renderConfigError err)
        Right config -> run config

serve :: Config -> IO ()
serve config = do
    resolved <- resolveDirectory (collectionsDirectory config)
    case resolved of
        Nothing -> failWith "The collections directory cannot be opened."
        Just directory -> do
            served <- try @IOException (Server.serve config directory (Text.hPutStrLn stderr))
            case served of
                Left err -> failWith ("The server stopped: " <> Text.pack (show err))
                Right () -> pure ()

createOwnerUrl :: Config -> IO ()
createOwnerUrl config = do
    resolved <- resolveDirectory (collectionsDirectory config)
    case resolved of
        Nothing -> failWith "The collections directory cannot be opened."
        Just directory -> do
            created <- createCollection directory
            case created of
                Left err -> failWith (creationFailure err)
                Right bearer -> Text.putStr (ownerMessage (configPublicOrigin config) bearer)

ownerMessage :: PublicOrigin -> Bearer -> Text
ownerMessage origin bearer =
    Text.unlines
        [ "Created a collection. Its owner URL grants full access to it and cannot be shown again:"
        , ""
        , renderPublicOrigin origin <> "/b/" <> renderBearer bearer
        ]

creationFailure :: CreateError -> Text
creationFailure = \case
    CollectionExists -> "A collection file with the new identifier already exists; nothing was changed."
    CreateUnavailable -> "The collections directory cannot hold a new collection file."
    CreateFault -> "Creating the collection failed, and no collection file was kept."

failWith :: Text -> IO ()
failWith message = Text.hPutStrLn stderr message >> exitFailure
