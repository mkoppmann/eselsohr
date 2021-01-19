module Lib.Core.Effect.Time
  ( MonadTime (..),
  )
where

import Data.Time (UTCTime)
import UnliftIO (MonadUnliftIO)

class (MonadUnliftIO m) => MonadTime m where
  getCurrentTime :: m UTCTime
