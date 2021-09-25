module Lib.Core.Domain.Id
  ( Id(Id)
  , mkNilId
  , toText
  ) where

import           Codec.Serialise.Class          ( Serialise )
import           Codec.Serialise.UUID           ( )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.UUID                      ( UUID
                                                , nil
                                                )
import           Prelude                 hiding ( toText )
import           Web.HttpApiData                ( FromHttpApiData
                                                , ToHttpApiData
                                                )

newtype Id a = Id {unId :: UUID}
  deriving
    (Eq, Hashable, Serialise, Read, Show, Ord, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)
    via UUID

mkNilId :: Id a
mkNilId = Id nil

toText :: Id a -> Text
toText = show @Text . unId
