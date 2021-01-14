module Lib.Core.Effect.Random
  ( MonadRandom (..),
  )
where

import Lib.Core.Domain (Id)
import UnliftIO (MonadUnliftIO)

class (MonadUnliftIO m) => MonadRandom m where
  getRandomId :: m (Id a)
