module Lib.Core.Effect.Random
  ( MonadRandom(..)
  ) where

import           Lib.App                        ( App )
import           Lib.Core.Domain                ( Id )
import qualified Lib.Impl.Random               as Impl

class (Monad m) => MonadRandom m where
  getRandomId :: m (Id a)

instance MonadRandom App where
  getRandomId = Impl.getRandomId
