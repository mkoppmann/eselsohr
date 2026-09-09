module Main (main) where

import Control.Monad (unless)
import Eselsohr.Version (version)
import System.Exit (die)

main :: IO ()
main = do
    let expectedVersion = "0.1.0.0"
    unless (version == expectedVersion) $
        die ("FAIL: expected version " ++ expectedVersion ++ ", got " ++ version)
    putStrLn "PASS: package version (1 assertion)"
