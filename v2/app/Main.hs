{- |
Module      : Main
Description : Command entry point

@--version@ prints the package name and version.
@serve@ loads the configuration from the environment and runs the web server until the process ends.
A configuration error, or a listener that cannot be resolved or bound, is reported on standard error with a failing exit status before any request is served.
Other arguments produce a usage message on standard error and a failing exit status.
-}
module Main (main) where

import Control.Exception (IOException, try)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Eselsohr.Config (loadConfig, renderConfigError)
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
        ["serve"] -> serve
        _ -> do
            hPutStrLn stderr "Usage: eselsohr-v2 serve | eselsohr-v2 --version"
            exitFailure

serve :: IO ()
serve = do
    loaded <- loadConfig lookupEnv
    case loaded of
        Left err -> failWith (renderConfigError err)
        Right config -> do
            served <- try @IOException (Server.serve config (Text.hPutStrLn stderr))
            case served of
                Left err -> failWith ("The server stopped: " <> Text.pack (show err))
                Right () -> pure ()
  where
    failWith message = Text.hPutStrLn stderr message >> exitFailure
