module Lib.Core.Domain.Resource
  ( Resource(..)
  , ArticleCollection(..)
  ) where

import           Codec.Serialise.Class          ( Serialise )
import           Lib.Core.Domain.Article        ( Article )
import           Lib.Core.Domain.Capability     ( Capability )
import           Lib.Core.Domain.Id             ( Id )

newtype Resource = ArticleResource ArticleCollection
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise)

data ArticleCollection = ArticleCollection
  { artCollection    :: !(HashMap (Id Article) Article)
  , artCapCollection :: !(HashMap (Id Capability) Capability)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass Serialise
