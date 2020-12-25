module Lib.Core.Capability
  ( Capability (..),
  )
where

import Database.SQLite.Simple.FromRow (FromRow, field, fromRow)
import Database.SQLite.Simple.ToRow (ToRow, toRow)
import Lib.Core.Id (AnyId, Id (..))
import Lib.Core.UserAction (UserAction)

data Capability = Capability
  { -- | Id of this entity
    capabilityId :: !(Id Capability),
    -- | Optional id of the entity this action works on. Must be the same id as
    -- in the action. Needed for easier SQL filtering
    capabilityEntity :: !(Maybe AnyId),
    -- | The encoded user action for this capability
    capabilityAction :: !UserAction
  }
  deriving stock (Show)

instance FromRow Capability where
  fromRow = Capability <$> field <*> field <*> field

instance ToRow Capability where
  toRow (Capability cId cEntity cAction) =
    toRow (cId, cEntity, cAction)
