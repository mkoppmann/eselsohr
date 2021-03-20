module Lib.Core.Effect.Time
  ( MonadTime (..),
  )
where

import Data.Time (UTCTime)

class (Monad m) => MonadTime m where
  getCurrentTime :: m UTCTime
