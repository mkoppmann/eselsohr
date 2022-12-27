module Main where

import Test.Tasty
    ( defaultMain
    , testGroup
    )

import Test.Integration (integrations)
import Test.Mock (mocks)
import Test.Property (props)
import Test.Unit (specs)

main :: IO ()
main = do
    unit <- specs
    mock <- mocks
    integration <- integrations
    defaultMain $ testGroup "Tests" [unit, props, mock, integration]
