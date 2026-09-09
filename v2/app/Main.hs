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
