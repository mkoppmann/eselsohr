module Lib.Core.Article
  ( Article (..),
    ArticleState (..),
  )
where

import Data.Time (UTCTime)
import Database.SQLite.Simple (SQLData (SQLText))
import Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed), fromField, returnError)
import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField, toField)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Lib.Core.Id (Id)
import Lib.Core.Uri (Uri)

data ArticleState = Unread | Archived deriving stock (Eq, Read, Show)

data Article = Article
  { articleId :: !(Id Article),
    articleTitle :: !LText,
    articleUrl :: !Uri,
    articleState :: !ArticleState,
    articleCreation :: !UTCTime
  }

instance FromField ArticleState where
  fromField f@(Field (SQLText t) _) =
    case readMaybe $ toString t of
      Just aState -> Ok aState
      Nothing -> returnError ConversionFailed f "Not an article state"
  fromField f = returnError ConversionFailed f "Error converting article state"

instance ToField ArticleState where
  toField = SQLText . show @Text

instance FromRow Article where
  fromRow = Article <$> field <*> field <*> field <*> field <*> field

instance ToRow Article where
  toRow (Article aId aTitle aUrl aState aCreation) = toRow (aId, aTitle, aUrl, aState, aCreation)
