module Lib.Core.Capability
  ( Capability (..),
  )
where

import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Lib.Core.Id (AnyId, Id (..))
import Lib.Core.UserAction (UserAction)

data Capability = Capability
  { capabilityId :: !(Id Capability),
    capabilityEntity :: !(Maybe AnyId),
    capabilityAction :: !UserAction
  }
  deriving stock (Show)

instance FromRow Capability where
  fromRow = Capability <$> field <*> field <*> field

instance ToRow Capability where
  toRow (Capability cId cEntity cAction) = toRow (cId, cEntity, cAction)
