{- |
Module      : Main
Description : Version command entry point

The executable accepts @--version@ and prints the package name and version.
Other arguments produce a usage message on standard error and a failing exit status.
-}
module Main (main) where

import Eselsohr.Version (version)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn ("eselsohr-v2 " ++ version)
        _ -> do
            hPutStrLn stderr "Usage: eselsohr-v2 --version"
            exitFailure
