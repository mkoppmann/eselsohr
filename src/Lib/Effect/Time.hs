module Lib.Effect.Time
  ( MonadTime (..),
  )
where

import qualified Data.Time as Time
import Lib.App (App)

class Monad m => MonadTime m where
  getCurrentTime :: m Time.UTCTime

instance MonadTime App where
  getCurrentTime = getCurrentTimeImpl

type WithTime m = MonadIO m

getCurrentTimeImpl :: WithTime m => m Time.UTCTime
getCurrentTimeImpl = liftIO Time.getCurrentTime
