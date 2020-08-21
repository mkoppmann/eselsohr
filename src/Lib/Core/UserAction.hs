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
  = CreateCollection
  deriving stock (Eq, Generic, Show)

instance Binary CollectionAction

data ArticleAction
  = InsertArticle
  | ListArticles
  | ShowArticle !(Id Article)
  | EditArticle !(Id Article)
  | ChangeArticleTitle !(Id Article)
  | ArchiveArticle !(Id Article)
  | UnreadArticle !(Id Article)
  | DeleteArticle !(Id Article)
  deriving stock (Eq, Generic, Show)

instance Binary ArticleAction

data UserAction
  = UaArticleAction !ArticleAction
  | UaCollectionAction !CollectionAction
  deriving stock (Eq, Generic, Show)

instance Binary UserAction

instance FromField UserAction where
  fromField (Field (SQLBlob blob) _) = Ok . decode . LB.fromChunks $ [blob]
  fromField f = returnError ConversionFailed f "Error converting capability action"

instance ToField UserAction where
  toField = SQLBlob . SB.concat . LB.toChunks . encode
