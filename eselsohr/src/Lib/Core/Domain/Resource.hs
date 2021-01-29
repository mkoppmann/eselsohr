module Lib.Core.Domain.Resource
  ( Resource (..),
    ArticleCollection (..),
  )
where

import Data.Binary (Binary)
import Lib.Core.Domain.Article (Article)
import Lib.Core.Domain.Capability (Action, Capability)
import Lib.Core.Domain.Id (Id)

newtype Resource = ArticleResource ArticleCollection
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Binary)

data ArticleCollection = ArticleCollection
  { artCapCollection :: !(Map (Id Capability) Capability),
    artActCollection :: !(Map (Id Action) Action),
    artCollection :: !(Map (Id Article) Article)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Binary)
