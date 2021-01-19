module Lib.Core.Domain.Id
  ( Id (Id),
    mkNilId,
    toText,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.UUID (UUID, nil)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Prelude hiding (toText)

newtype Id a = Id {unId :: UUID}
  deriving
    (Binary, Eq, Read, Show, Ord, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)
    via UUID

mkNilId :: Id a
mkNilId = Id nil

toText :: Id a -> Text
toText = show @Text . unId
