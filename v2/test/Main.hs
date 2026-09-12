{- |
Module      : Main
Description : Domain and policy test suite entry point

The Tasty suite combines the package version check with tests for validated values, article edits, grant inclusion, delegation, ancestry, and operation policy.
Each test module contributes its own test group.
-}
module Main (main) where

import Articles (articleTests)
import Attenuation (attenuationTests)
import Authorization (authorizationTests)
import Capabilities (capabilityTests)
import Chains (chainTests)
import Eselsohr.Version (version)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Values (valueTests)

main :: IO ()
main = defaultMain $ testGroup "Eselsohr" [testCase "package version" (version @?= "0.1.0.0"), valueTests, articleTests, capabilityTests, attenuationTests, chainTests, authorizationTests]
