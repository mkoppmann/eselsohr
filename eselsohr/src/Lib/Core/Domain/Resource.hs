module Lib.Core.Domain.Resource
  ( Resource(..)
  , ArticleCollection(..)
  ) where

import           Codec.Serialise.Class          ( Serialise )
import           Lib.Core.Domain.Article        ( Article )
import           Lib.Core.Domain.Capability     ( Action
                                                , Capability
                                                )
import           Lib.Core.Domain.Id             ( Id )

newtype Resource = ArticleResource ArticleCollection
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise)

data ArticleCollection = ArticleCollection
  { artCapCollection :: !(HashMap (Id Capability) Capability)
  , artActCollection :: !(HashMap (Id Action) Action)
  , artCollection    :: !(HashMap (Id Article) Article)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass Serialise
