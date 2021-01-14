module Lib.Impl.Random
  ( getRandomId,
  )
where

import qualified Data.UUID.V4 as UUID
import Lib.Core.Domain (Id (Id))

type WithRandom m = MonadIO m

getRandomId :: WithRandom m => m (Id a)
getRandomId = Id <$> liftIO UUID.nextRandom
