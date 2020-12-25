module Lib.Core.UserAction
  ( UserAction (..),
    ArticleAction (..),
    CollectionAction (..),
  )
where

import Data.Binary (Binary, decode, encode)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Database.SQLite.Simple (ResultError (ConversionFailed), SQLData (SQLBlob))
import Database.SQLite.Simple.FromField (FromField (fromField), returnError)
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (toField))
import Lib.Core.Article (Article)
import Lib.Core.Id (Id)

data CollectionAction
  = -- | Effect: Creates a new collection database
    CreateCollection
  | -- | Effect: Unlocks collection and creates new accesstoken for
    -- 'ListArticles' capability
    CreateListArticlesAcc
  | -- | Effect: Delete the matching 'ListArticles' accesstoken stored in the
    -- capability and then deletes itself
    DeleteListArticles
  | -- | Gui: Show collection main page
    OverviewCollection
  deriving stock (Eq, Generic, Show)

instance Binary CollectionAction

data ArticleAction
  = -- | Effect: Inserts an 'Article' into the database
    InsertArticle
  | -- | Effect: Change the article’s title
    ChangeArticleTitle !(Id Article)
  | -- | Effect: Set the state of the article to 'Archived'
    ArchiveArticle !(Id Article)
  | -- | Effect: Set the state of the article to 'Unread'
    UnreadArticle !(Id Article)
  | -- | Effect: Delete the article
    DeleteArticle !(Id Article)
  | -- | Gui: Show article overview
    ListArticles
  | -- | Gui: Show the article’s detail page
    ShowArticle !(Id Article)
  | -- | Gui: Show the article’s edit page
    EditArticle !(Id Article)
  deriving stock (Eq, Generic, Show)

instance Binary ArticleAction

data UserAction
  = UaArtAction !ArticleAction
  | UaColAction !CollectionAction
  deriving stock (Eq, Generic, Show)

instance Binary UserAction

instance FromField UserAction where
  fromField (Field (SQLBlob blob) _) = Ok . decode . LB.fromChunks $ [blob]
  fromField f = returnError ConversionFailed f "Error converting capability action"

instance ToField UserAction where
  toField = SQLBlob . SB.concat . LB.toChunks . encode
