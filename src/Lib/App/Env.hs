{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env
  ( DataPath,
    Env (..),
    Has (..),
    grab,
  )
where

import Colog (HasLog (..), LogAction, Message)
import Network.Wai.Handler.Warp (Port)

type DataPath = FilePath

data Env (m :: Type -> Type) = Env
  { envDataFolder :: !DataPath,
    envLogAction :: !(LogAction m Message),
    envServerPort :: !Port
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

instance Has (LogAction m Message) (Env m) where obtain = envLogAction

instance Has Port (Env m) where obtain = envServerPort

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
