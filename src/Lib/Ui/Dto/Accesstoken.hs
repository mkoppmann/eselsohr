module Lib.Ui.Dto.Accesstoken
    ( Accesstoken
    , Reference (..)
    , mkAccesstoken
    , toReference
    , encodeToBase32
    , decodeFromBase32
    ) where

import Codec.Serialise qualified as Ser
import Text.Show qualified

import Codec.Serialise.Class (Serialise)
import Codec.Serialise.UUID ()
import Data.ByteString.Lazy.Base32
    ( decodeBase32
    , encodeBase32Unpadded'
    )
import Data.UUID (UUID)
import Web.HttpApiData
    ( FromHttpApiData (..)
    , ToHttpApiData (..)
    )

import Lib.Domain.Id qualified as Id

import Lib.Domain.Capability (Capability)
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)

data Reference = Reference
    { collectionId :: !(Id Collection)
    , capabilityId :: !(Id Capability)
    }
    deriving stock (Show, Eq)

------------------------------------------------------------------------
-- DTO
------------------------------------------------------------------------

data ReferenceDto
    = ReferenceDtoInit !ReferenceDtoV1Data
    | ReferenceDtoV1 !ReferenceDtoV1Data
    deriving stock (Generic)
    deriving anyclass (Serialise)

data ReferenceDtoV1Data = ReferenceDtoV1Data
    { v1CollectionIdUuid :: !UUID
    , v1CapabilityIdUuid :: !UUID
    }
    deriving stock (Generic)
    deriving anyclass (Serialise)

fromDomain :: Reference -> ReferenceDto
fromDomain Reference{..} = ReferenceDtoV1 $ ReferenceDtoV1Data (Id.toUuid collectionId) (Id.toUuid capabilityId)

toDomain :: ReferenceDto -> Reference
toDomain ref = case ref of
    ReferenceDtoV1 v1Data ->
        Reference (Id.fromUuid v1Data.v1CollectionIdUuid) (Id.fromUuid v1Data.v1CapabilityIdUuid)
    _otherVersion -> toDomain $ migrate ref

migrate :: ReferenceDto -> ReferenceDto
migrate = \case
    ReferenceDtoInit initData -> migrate $ ReferenceDtoV1 initData
    ReferenceDtoV1 v1Data -> ReferenceDtoV1 v1Data

------------------------------------------------------------------------
-- Accesstoken
------------------------------------------------------------------------

newtype Accesstoken = Accesstoken {unAccesstoken :: LByteString}
    deriving (Eq) via LByteString

instance Show Accesstoken where
    show = toString . toUrlPiece

instance ToHttpApiData Accesstoken where
    toUrlPiece = encodeToBase32

instance FromHttpApiData Accesstoken where
    parseUrlPiece = decodeFromBase32

mkAccesstoken :: Reference -> Accesstoken
mkAccesstoken = Accesstoken . Ser.serialise . fromDomain

toReference :: Accesstoken -> Reference
toReference acc = toDomain $ Ser.deserialise acc.unAccesstoken

encodeToBase32 :: Accesstoken -> Text
encodeToBase32 acc = decodeUtf8 $ encodeBase32Unpadded' acc.unAccesstoken

decodeFromBase32 :: Text -> Either Text Accesstoken
decodeFromBase32 = either (const $ Left "invalid UrlToken") (Right . Accesstoken) . decodeBase32 . encodeUtf8
