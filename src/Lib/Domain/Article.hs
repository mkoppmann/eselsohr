module Lib.Domain.Article
  ( Article(..)
  , ArticleState(..)
  , changeTitle
  , markAsUnread
  , markAsRead
  , titleFromText
  ) where

import           Data.Time.Clock                                      ( UTCTime )
import           Prelude                                       hiding ( id
                                                                      , state
                                                                      )

import qualified Lib.Domain.NonEmptyText                             as NET

import           Lib.Domain.Error                                     ( AppErrorType )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.NonEmptyText                              ( NonEmptyText )
import           Lib.Domain.Uri                                       ( Uri )

data Article = Article
  { id       :: !(Id Article)
  , title    :: !NonEmptyText
  , uri      :: !Uri
  , state    :: !ArticleState
  , creation :: !UTCTime
  }
  deriving stock Show

instance Eq Article where
  (==) (Article aId _ _ _ _) (Article bId _ _ _ _) = aId == bId

data ArticleState
  = Unread
  | Read
  deriving stock (Eq, Read, Show)

changeTitle :: NonEmptyText -> Article -> Article
changeTitle newTitle art = art { title = newTitle }

markAsUnread :: Article -> Article
markAsUnread art = art { state = Unread }

markAsRead :: Article -> Article
markAsRead art = art { state = Read }

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

titleFromText :: Text -> Either AppErrorType NonEmptyText
titleFromText title = NET.fromText title "Article title cannot be empty"
