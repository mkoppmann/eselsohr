{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Ui.Dto.Id where

import Data.UUID (UUID)
import Web.HttpApiData
    ( FromHttpApiData
    , ToHttpApiData
    )

import Lib.Domain.Id (Id (..))

deriving via UUID instance FromHttpApiData (Id a)
deriving via UUID instance ToHttpApiData (Id a)
