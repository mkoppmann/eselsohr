module Lib.Domain.Id
  ( Id(..)
  , mkNilId
  , fromUuid
  , toUuid
  ) where

import           Data.UUID                                            ( UUID
                                                                      , nil
                                                                      )

newtype Id a = Id {unId :: UUID}
  deriving (Eq, Ord, Read, Show) via UUID

instance ToText (Id a) where
  toText = show . unId

mkNilId :: Id a
mkNilId = coerce nil

fromUuid :: UUID -> Id a
fromUuid = coerce

toUuid :: Id a -> UUID
toUuid = coerce
