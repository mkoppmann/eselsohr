{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Data.Time (UTCTime)
import Data.UUID (UUID, fromText, toText)
import Database.SQLite.Simple (FromRow, ResultError (ConversionFailed), SQLData (SQLText), ToRow, field, fromRow, toRow)
import Database.SQLite.Simple.FromField (FromField, fromField, returnError)
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField, toField)
import Prelude hiding (toText)

newtype SqliteUUID = SqliteUUID {getSqliteUUID :: UUID}
  deriving newtype (Eq, Read)

instance FromField SqliteUUID where
  fromField f@(Field (SQLText t) _) =
    case fromText t of
      Just uuid -> Ok $ SqliteUUID uuid
      Nothing -> returnError ConversionFailed f "not a valid UUID"
  fromField f = returnError ConversionFailed f "error converting UUID"

instance ToField SqliteUUID where
  toField = SQLText . toText . getSqliteUUID
  {-# INLINE toField #-}

newtype Accesstoken = Accesstoken {getAccesstoken :: SqliteUUID}
  deriving newtype (FromField, ToField, Eq, Read)

newtype CollectionId = CollectionId {getCollectionId :: SqliteUUID}
  deriving newtype (FromField, ToField, Eq, Read)

data Actions = PATCH | DELETE deriving stock (Eq, Read, Show)

data CollectionMapping = CollectionMapping
  { collectionMappingId :: CollectionId,
    collectionMappingAccesstoken :: Accesstoken
  }
  deriving stock (Eq, Read)

instance FromRow CollectionMapping where
  fromRow = CollectionMapping <$> field <*> field

instance ToRow CollectionMapping where
  toRow (CollectionMapping collId acc) = toRow (collId, acc)

data Article = Article
  { articleId :: SqliteUUID,
    articleTitle :: LText,
    articleHref :: LText,
    articleCreate_at :: UTCTime
  }
  deriving stock (Eq, Read)

instance FromRow Article where
  fromRow = Article <$> field <*> field <*> field <*> field

instance ToRow Article where
  toRow (Article aID aTitle aHref aCreated) = toRow (aID, aTitle, aHref, aCreated)
