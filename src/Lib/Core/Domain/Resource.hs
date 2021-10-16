module Lib.Core.Domain.Resource
  ( Resource(..)
  ) where

import           Codec.Serialise.Class          ( Serialise )
import           Lib.Core.Domain.Article        ( Article )
import           Lib.Core.Domain.Capability     ( Capability )
import           Lib.Core.Domain.Id             ( Id )

data Resource = Resource
  { artCollection :: !(Map (Id Article) Article)
  , capCollection :: !(Map (Id Capability) Capability)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass Serialise
