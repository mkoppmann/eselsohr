module Lib.Core.Domain.Article
  ( Article (..),
    ArticleState (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Binary.Instances.Time ()
import Data.Time (UTCTime)
import Lib.Core.Domain.Entity (Entity)
import Lib.Core.Domain.Uri (Uri)

data ArticleState
  = Unread
  | Archived
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Binary, FromJSON, ToJSON)

data Article = Article
  { title :: !Text,
    uri :: !Uri,
    state :: !ArticleState,
    creation :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary, FromJSON, ToJSON)

instance Binary (Entity Article)
