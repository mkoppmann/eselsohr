module Lib.Core.Uuid
  ( Uuid (..),
  )
where

import Data.Binary (Binary)
import qualified Data.UUID as U
import Database.SQLite.Simple (ResultError (ConversionFailed), SQLData (SQLText))
import Database.SQLite.Simple.FromField (FromField, fromField, returnError)
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField, toField)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype Uuid = Uuid {unUuid :: U.UUID}
  deriving (Binary, Eq, FromHttpApiData, ToHttpApiData, Read, Show) via U.UUID

instance FromField Uuid where
  fromField f@(Field (SQLText t) _) =
    case U.fromText t of
      Just uuid -> Ok $ Uuid uuid
      Nothing -> returnError ConversionFailed f "Not a valid UUID"
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance ToField Uuid where
  toField = SQLText . U.toText . unUuid
  {-# INLINE toField #-}

