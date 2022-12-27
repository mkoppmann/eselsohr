module Test.Mock
    ( mocks
    ) where

import Test.Tasty
    ( TestTree
    , testGroup
    )
import Test.Tasty.Hspec (testSpec)

import Test.App.Command (commandMockedSpecs)

mocks :: IO TestTree
mocks = do
    command <- testSpec "CommandMockedSpec" commandMockedSpecs
    pure $ testGroup "Mocked tests" [command]
