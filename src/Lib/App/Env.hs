{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env
    ( DataPath
    , BaseUrl
    , WriteQueue
    , Https (..)
    , Hsts (..)
    , DeploymentMode (..)
    , CollectionCreation (..)
    , Env (..)
    , Has (..)
    , grab
    , HasWriteQueue (..)
    , envWriteQueue
    ) where

import Colog
    ( HasLog (..)
    , LogAction
    , Message
    )
import UnliftIO.STM (TQueue)

import Lib.Domain.Repo (RepositoryCommandSync)
import Lib.Domain.Uri (Uri)

type DataPath = FilePath

type BaseUrl = Uri

type WriteQueue m = TQueue (RepositoryCommandSync m)

data Https = HttpsOn | HttpsOff

data Hsts = HstsOn | HstsOff

data DeploymentMode = Prod | Test | Dev
    deriving stock (Read)

data CollectionCreation = Public | Private

data Env (m :: Type -> Type) = Env
    { dataFolder :: !DataPath
    , baseUrl :: !BaseUrl
    , writeQueue :: !(WriteQueue m)
    , logAction :: !(LogAction m Message)
    , https :: !Https
    , hsts :: !Hsts
    , deploymentMode :: !DeploymentMode
    , collectionCreation :: !CollectionCreation
    }

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = logAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env{logAction = newAction}
    {-# INLINE setLogAction #-}

class Has field env where
    obtain :: env -> field

instance Has DataPath (Env m) where
    obtain = dataFolder

instance Has BaseUrl (Env m) where
    obtain = baseUrl

instance Has (WriteQueue m) (Env m) where
    obtain = writeQueue

instance Has (LogAction m Message) (Env m) where
    obtain = logAction

instance Has Https (Env m) where
    obtain = https

instance Has Hsts (Env m) where
    obtain = hsts

instance Has DeploymentMode (Env m) where
    obtain = deploymentMode

instance Has CollectionCreation (Env m) where
    obtain = collectionCreation

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

class HasWriteQueue env m where
    getWriteQueue :: env -> WriteQueue m

instance HasWriteQueue (Env m) m where
    getWriteQueue = writeQueue

envWriteQueue :: forall env m. (MonadReader env m, HasWriteQueue env m) => m (WriteQueue m)
envWriteQueue = asks getWriteQueue
