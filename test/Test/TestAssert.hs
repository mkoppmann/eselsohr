{- | We use @hspec@ testing framework and it doesn't support monad transformers.
 At this moment there's no testing framework that supports monad transformers. So
 we need to pass @TestAppEnv@ manually to every function.
 All functions take `TestAppEnv` as last argument. Like this one:

 @
 satisfies :: App a -> (a -> Bool) -> TestAppEnv -> Expectation
 @

 Because of that, there're multiple ways to write tests:

 @
 1. satisfies action isJust env
 2. (action `satisfies` isJust) env
 3. action `satisfies` isJust $ env
 4. env & action `satisfies` isJust
 @

 We can go even further and introduce fancy operators, so this code can be
 written in more concise way. But this is TBD.

 @
 env & action @? isJust
 @
-}
module Test.TestAssert
    ( succeeds
    , satisfies
    , failsWith
    , failsWithEither
    , equals
    , redirects
    , defaultTestEnv
    , runTestApp
    ) where

import Test.Hspec
    ( Expectation
    , expectationFailure
    , shouldBe
    , shouldSatisfy
    )

import Lib.Domain.Error
    ( AppErrorType
    , isRedirect
    )
import Lib.Infra.Error (AppError (..))
import Lib.Infra.Log
    ( Severity (..)
    , mainLogAction
    )
import Lib.Infra.Persistence.Model.Collection (mkCollection)
import Test.App.Env (TestEnv (..))
import Test.Infra.Monad
    ( TestApp
    , TestAppEnv
    , runAppAsIO
    )

-- | Checks that given action runs successfully.
succeeds :: (Show a) => TestApp a -> TestAppEnv -> Expectation
succeeds = (`satisfies` const True)

-- | Checks whether return result of the action satisfies given predicate.
satisfies :: (Show a) => TestApp a -> (a -> Bool) -> TestAppEnv -> Expectation
satisfies app p env =
    runAppAsIO env app >>= \case
        Left e -> expectationFailure $ "Expected 'Success' but got: " <> show e
        Right a -> a `shouldSatisfy` p

-- | Checks whether action fails and returns given error.
failsWith :: (Show a) => TestApp a -> AppErrorType -> TestAppEnv -> Expectation
failsWith app err env =
    runAppAsIO env app >>= \case
        Left AppError{..} -> appErrorType `shouldBe` err
        Right a -> expectationFailure $ "Expected 'Failure' with: " <> show err <> " but got: " <> show a

-- | Checks whether action fails with an Either and returns given error.
failsWithEither
    :: (Eq a, Show a) => TestApp (Either AppErrorType a) -> Either AppErrorType a -> TestAppEnv -> Expectation
failsWithEither app err env =
    runAppAsIO env app >>= \case
        Left AppError{..} -> Left appErrorType `shouldBe` err
        Right result -> case result of
            Left failure -> Left failure `shouldBe` err
            Right a -> expectationFailure $ "Expected 'Failure' with: " <> show err <> " but got: " <> show a

-- | Checks whether action returns expected value.
equals :: (Show a, Eq a) => TestApp a -> a -> TestAppEnv -> Expectation
equals app v env =
    runAppAsIO env app >>= \case
        Right a -> a `shouldBe` v
        Left e -> expectationFailure $ "Expected 'Success' but got: " <> show e

-- | Checks whether action issues a redirect.
redirects :: (Show a) => TestApp a -> TestAppEnv -> Expectation
redirects app env =
    runAppAsIO env app >>= \case
        Left AppError{..} -> isRedirect appErrorType `shouldBe` True
        Right a -> expectationFailure $ "Expected redirect but got: " <> show a

-- | Provides a default 'TestAppEnv'.
defaultTestEnv :: (MonadIO m) => m TestAppEnv
defaultTestEnv = do
    let logAction = mainLogAction Debug
    collectionState <- newIORef mkCollection
    pure $ TestEnv{..}

-- | Runs application for its effects on the environment. Must succeed.
runTestApp :: TestAppEnv -> TestApp a -> IO a
runTestApp env app =
    runAppAsIO env app >>= \case
        Left err -> error $ "Could not run test app: " <> show err
        Right res -> pure res
