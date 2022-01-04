module Lib.Infra.Adapter.Random
  ( getRandomId
  ) where

import qualified Data.UUID.V4                                        as UUID

import qualified Lib.Domain.Id                                       as Id

import           Lib.Domain.Id                                        ( Id )

type WithRandom m = MonadIO m

getRandomId :: WithRandom m => m (Id a)
getRandomId = Id.fromUuid <$> liftIO UUID.nextRandom
