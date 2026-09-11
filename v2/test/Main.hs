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
