{-# LANGUAGE TypeSynonymInstances #-}

module Test.App.Env
    ( TestEnv (..)
    , CollectionState
    , Has (..)
    , grab
    ) where

import Colog
    ( HasLog (..)
    , LogAction
    , Message
    )

import Lib.Infra.Persistence.Model.Collection (CollectionPm)

type CollectionState = IORef CollectionPm

data TestEnv (m :: Type -> Type) = TestEnv
    { logAction :: !(LogAction m Message)
    , collectionState :: !CollectionState
    }

instance HasLog (TestEnv m) Message m where
    getLogAction :: TestEnv m -> LogAction m Message
    getLogAction env = env.logAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> TestEnv m -> TestEnv m
    setLogAction newAction env = env{logAction = newAction}
    {-# INLINE setLogAction #-}

class Has field env where
    obtain :: env -> field

instance Has (LogAction m Message) (TestEnv m) where
    obtain env = env.logAction

instance Has CollectionState (TestEnv m) where
    obtain env = env.collectionState

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
