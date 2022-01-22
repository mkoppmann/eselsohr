module Lib.Ui.Dto.Accesstoken
  ( Accesstoken
  , Reference(..)
  , mkAccesstoken
  , toReference
  , encodeToBase32
  , decodeFromBase32
  ) where

import qualified Codec.Serialise                                     as Ser
import qualified Text.Show

import           Codec.Serialise.Class                                ( Serialise )
import           Codec.Serialise.UUID                                 ( )
import           Data.ByteString.Lazy.Base32                          ( decodeBase32
                                                                      , encodeBase32Unpadded'
                                                                      )
import           Data.UUID                                            ( UUID )
import           Web.HttpApiData                                      ( FromHttpApiData(..)
                                                                      , ToHttpApiData(..)
                                                                      )

import qualified Lib.Domain.Id                                       as Id

import           Lib.Domain.Capability                                ( Capability )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )

data Reference = Reference
  { collectionId :: !(Id Collection)
  , capabilityId :: !(Id Capability)
  }
  deriving stock (Show, Eq)

data ReferenceDto = ReferenceDto
  { collectionIdUuid :: !UUID
  , capbilityIdUuid  :: !UUID
  }
  deriving stock Generic
  deriving anyclass Serialise

fromDomain :: Reference -> ReferenceDto
fromDomain Reference {..} = ReferenceDto (Id.toUuid collectionId) (Id.toUuid capabilityId)

toDomain :: ReferenceDto -> Reference
toDomain ReferenceDto {..} = Reference (Id.fromUuid collectionIdUuid) (Id.fromUuid capbilityIdUuid)

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
toReference = toDomain . Ser.deserialise . unAccesstoken

encodeToBase32 :: Accesstoken -> Text
encodeToBase32 = decodeUtf8 . encodeBase32Unpadded' . unAccesstoken

decodeFromBase32 :: Text -> Either Text Accesstoken
decodeFromBase32 = either (const $ Left "invalid UrlToken") (Right . Accesstoken) . decodeBase32 . encodeUtf8
