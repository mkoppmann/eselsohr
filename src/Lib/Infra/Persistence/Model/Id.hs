{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Infra.Persistence.Model.Id where

import Data.Aeson.Types
    ( FromJSON
    , FromJSONKey
    , ToJSON
    , ToJSONKey
    )
import Data.UUID (UUID)

import Lib.Domain.Id (Id)
import qualified Lib.Domain.Id as Domain

deriving via UUID instance FromJSON (Id a)
deriving via UUID instance FromJSONKey (Id a)
deriving via UUID instance ToJSON (Id a)
deriving via UUID instance ToJSONKey (Id a)
