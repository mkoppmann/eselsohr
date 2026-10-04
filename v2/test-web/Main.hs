{- |
Module      : Main
Description : Web interface test suite entry point

The Tasty suite checks configuration parsing and loading, the route table and fixed pages, the pure bearer page, the bearer page and the add form through the whole application with real collections, and the application and Warp settings over real local connections.
-}
module Main (main) where

import AdditionTests (additionTests)
import BearerPageTests (bearerPageTests)
import ConfigTests (configTests)
import PageTests (pageTests)
import RouteTests (routeTests)
import ServerTests (serverTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Web" [configTests, routeTests, pageTests, bearerPageTests, additionTests, serverTests]
