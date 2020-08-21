module Lib.Core.Accesstoken
  ( Accesstoken (..),
  )
where

import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Lib.Core.Capability (Capability)
import Lib.Core.Collection (Collection)
import Lib.Core.Id (Id)

data Accesstoken = Accesstoken
  { accesstokenId :: !(Id Accesstoken),
    accesstokenCol :: !(Id Collection),
    accesstokenCap :: !(Id Capability)
  }
  deriving stock (Show)

instance FromRow Accesstoken where
  fromRow = Accesstoken <$> field <*> field <*> field

instance ToRow Accesstoken where
  toRow (Accesstoken aId aCol aCap) = toRow (aId, aCol, aCap)
