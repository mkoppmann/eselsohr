module Lib.Infra.Adapter.Time
  ( getCurrentTime
  ) where

import qualified Data.Time                                           as Time

import           Data.Time.Clock                                      ( UTCTime )

type WithTime m = MonadIO m

getCurrentTime :: WithTime m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime
