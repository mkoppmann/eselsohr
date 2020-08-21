{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Lib.Core.Id
  ( Id (..),
    AnyId,
    castId,
  )
where

import Data.Binary (Binary)
import Data.Type.Equality (type (==))
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Lib.Core.Uuid (Uuid (Uuid))
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype Id a = Id {unId :: Uuid}
  deriving (Binary, Eq, FromHttpApiData, ToHttpApiData, Read, Show, FromField, ToField) via Uuid

-- | For when we don’t care about the type
type AnyId = Id ()

-- | Unsafe cast of 'Id'. Implementation uses smart trick to enforce usage
-- always with @TypeApplications@.
castId :: forall to from to'. ((to == to') ~ 'True) => Id from -> Id to'
castId (Id a) = Id a
