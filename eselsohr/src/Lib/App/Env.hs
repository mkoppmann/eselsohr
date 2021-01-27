{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env
  ( DataPath,
    WriteQueue,
    MaxConcurrentWrites,
    Https (..),
    Hsts (..),
    Env (..),
    Has (..),
    grab,
  )
where

import Colog (HasLog (..), LogAction, Message)
import Lib.Core.Domain.StoreEvent (SynchronizedStoreEvent)
import Lib.Core.Domain.Uri (Uri)
import UnliftIO.STM (TQueue)

type DataPath = FilePath

type WriteQueue = TQueue SynchronizedStoreEvent

type MaxConcurrentWrites = Int

data Https = HttpsOn | HttpsOff

data Hsts = HstsOn | HstsOff

data Env (m :: Type -> Type) = Env
  { envDataFolder :: !DataPath,
    envWriteQueue :: !WriteQueue,
    envMaxConcurrentWrites :: !(Maybe MaxConcurrentWrites),
    envLogAction :: !(LogAction m Message),
    envBaseUrl :: !Uri,
    envHttps :: !Https,
    envHsts :: !Hsts
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newAction env = env {envLogAction = newAction}
  {-# INLINE setLogAction #-}

class Has field env where
  obtain :: env -> field

instance Has DataPath (Env m) where obtain = envDataFolder

instance Has WriteQueue (Env m) where obtain = envWriteQueue

instance Has (Maybe MaxConcurrentWrites) (Env m) where obtain = envMaxConcurrentWrites

instance Has (LogAction m Message) (Env m) where obtain = envLogAction

instance Has Uri (Env m) where obtain = envBaseUrl

instance Has Https (Env m) where obtain = envHttps

instance Has Hsts (Env m) where obtain = envHsts

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
