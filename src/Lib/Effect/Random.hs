module Lib.Effect.Random
  ( MonadRandom (..),
  )
where

import qualified Data.UUID.V4 as UUID
import Lib.App (App)
import Lib.Core.Id (Id (Id))
import Lib.Core.Uuid (Uuid (Uuid))

class Monad m => MonadRandom m where
  getRandomId :: m (Id a)

instance MonadRandom App where
  getRandomId = getRandomUuid

type WithRandom m = MonadIO m

getRandomUuid :: WithRandom m => m (Id a)
getRandomUuid = Id . Uuid <$> liftIO UUID.nextRandom
