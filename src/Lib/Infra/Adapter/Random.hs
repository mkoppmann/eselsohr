module Lib.Infra.Adapter.Random
    ( getRandomId
    ) where

import Data.UUID.V4 qualified as UUID

import Lib.Domain.Id qualified as Id

import Lib.Domain.Id (Id)

type WithRandom m = MonadIO m

getRandomId :: WithRandom m => m (Id a)
getRandomId = Id.fromUuid <$> liftIO UUID.nextRandom
