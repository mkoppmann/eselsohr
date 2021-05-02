module Lib.Core.Effect.Time
  ( MonadTime(..)
  ) where

import           Data.Time                      ( UTCTime )
import           Lib.App                        ( App )
import qualified Lib.Impl.Time                 as Impl

class (Monad m) => MonadTime m where
  getCurrentTime :: m UTCTime

instance MonadTime App where
  getCurrentTime = Impl.getCurrentTime
