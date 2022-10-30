module Lib.Domain.Id
  ( Id(..)
  , mkNilId
  , fromUuid
  , toUuid
  , fromText
  ) where

import qualified Data.UUID                                           as UUID

import           Data.UUID                                            ( UUID )

newtype Id a = Id {unId :: UUID}
  deriving (Eq, Ord, Read, Show) via UUID

instance ToText (Id a) where
  toText = show . unId

mkNilId :: Id a
mkNilId = coerce UUID.nil

fromUuid :: UUID -> Id a
fromUuid = coerce

toUuid :: Id a -> UUID
toUuid = coerce

fromText :: Text -> Maybe (Id a)
fromText = coerce . UUID.fromText
