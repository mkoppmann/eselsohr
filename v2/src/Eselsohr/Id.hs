{- |
Module      : Eselsohr.Id
Description : Distinct UUIDv4 identifiers for collections, articles, and grants

t'CollectionId', t'ArticleId', and t'CapabilityId' distinguish identifiers that would otherwise share the same UUID representation.
Their parsers accept canonical lowercase UUIDv4 text with hyphens and return 'InvalidId' for other input.
The corresponding renderers reproduce that canonical text.
-}
module Eselsohr.Id
    ( CollectionId
    , ArticleId
    , CapabilityId
    , IdError (..)
    , parseCollectionId
    , parseArticleId
    , parseCapabilityId
    , renderCollectionId
    , renderArticleId
    , renderCapabilityId
    ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID

newtype CollectionId = CollectionId UUID
    deriving stock (Eq, Ord, Show)

newtype ArticleId = ArticleId UUID
    deriving stock (Eq, Ord, Show)

newtype CapabilityId = CapabilityId UUID
    deriving stock (Eq, Ord, Show)

data IdError = InvalidId
    deriving stock (Eq, Show)

parseCollectionId :: Text -> Either IdError CollectionId
parseCollectionId = fmap CollectionId . parseUuid

parseArticleId :: Text -> Either IdError ArticleId
parseArticleId = fmap ArticleId . parseUuid

parseCapabilityId :: Text -> Either IdError CapabilityId
parseCapabilityId = fmap CapabilityId . parseUuid

renderCollectionId :: CollectionId -> Text
renderCollectionId (CollectionId uuid) = UUID.toText uuid

renderArticleId :: ArticleId -> Text
renderArticleId (ArticleId uuid) = UUID.toText uuid

renderCapabilityId :: CapabilityId -> Text
renderCapabilityId (CapabilityId uuid) = UUID.toText uuid

parseUuid :: Text -> Either IdError UUID
parseUuid input
    | Text.compareLength input 36 /= EQ = Left InvalidId
    | Text.take 1 (Text.drop 14 input) /= "4" = Left InvalidId
    | Text.take 1 (Text.drop 19 input) `notElem` ["8", "9", "a", "b"] = Left InvalidId
    | otherwise = case UUID.fromText input of
        Just uuid | UUID.toText uuid == input -> Right uuid
        _ -> Left InvalidId
