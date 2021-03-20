module Lib.Core.Effect.Random
  ( MonadRandom (..),
  )
where

import Lib.Core.Domain (Id)

class (Monad m) => MonadRandom m where
  getRandomId :: m (Id a)
