module Lib.Core.Domain.Accesstoken
  ( Accesstoken,
    Reference (..),
    Revocable,
    mkAccesstoken,
    toReference,
  )
where

import Data.Binary (Binary)
import qualified Data.Binary as Bin
import qualified Data.ByteString.Base64.URL.Lazy as B64
import Lib.Core.Domain.Capability (Capability)
import Lib.Core.Domain.Id (Id)
import Lib.Core.Domain.Resource (Resource)
import qualified Text.Show
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

data Reference = Reference
  { resourceId :: !(Id Resource),
    capabilityId :: !(Id Capability)
  }
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

newtype Accesstoken = Accesstoken {unAccesstoken :: LByteString}
  deriving stock (Generic)
  deriving anyclass (Binary)

instance Show Accesstoken where
  show = toString . toUrlPiece

instance ToHttpApiData Accesstoken where
  toUrlPiece = decodeUtf8 . B64.encode . unAccesstoken

instance FromHttpApiData Accesstoken where
  parseUrlPiece =
    either (const $ Left "invalid UrlToken") (Right . Accesstoken)
      . B64.decode
      . encodeUtf8

type Revocable = (Accesstoken, Accesstoken)

mkAccesstoken :: Reference -> Accesstoken
mkAccesstoken = Accesstoken . Bin.encode

toReference :: Accesstoken -> Reference
toReference = Bin.decode . unAccesstoken
