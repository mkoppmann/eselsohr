module Lib.Impl.Time
  ( getCurrentTime
  ) where

import           Data.Time                      ( UTCTime )
import qualified Data.Time                     as Time

type WithTime m = MonadIO m

getCurrentTime :: WithTime m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime
