{- |
Module      : Main
Description : Web interface test suite entry point

The Tasty suite checks configuration parsing and loading, the route table and fixed pages, and the application and Warp settings over real local connections.
-}
module Main (main) where

import ConfigTests (configTests)
import RouteTests (routeTests)
import ServerTests (serverTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Web" [configTests, routeTests, serverTests]
