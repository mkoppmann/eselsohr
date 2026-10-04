{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Eselsohr.Store.SQLite.Codec
Description : SQLite rows and checked conversions for articles and grants

t'ArticleRow', t'GrantRow', and t'PermissionRow' describe SQL columns and provide the row instances used by queries and writes.
Separate conversions validate their contents: 'fromArticleRow' produces a t'StoredArticle', and 'fromGrantRows' assembles a t'StoredGrant' from a grant row and its permissions.

Stored values retain revision metadata, token hashes, and petnames outside the domain records.
Encoding converts identifiers to UUID bytes and timestamps to exact signed 64-bit Unix microseconds, rejecting values that cannot be represented.

'catchConversionErrors' replaces row conversion exception details with a t'CodecError', so callers do not expose stored values through error messages.
-}
module Eselsohr.Store.SQLite.Codec
    ( -- * Stored values and SQL rows
      ArticleRow (..)
    , GrantRow (..)
    , PermissionRow (..)
    , StoredArticle (..)
    , StoredGrant (..)

      -- * Row conversions
    , toArticleRow
    , fromArticleRow
    , toGrantRows
    , fromGrantRows

      -- * Identifier encoding
    , toCollectionBytes
    , toArticleBytes
    , toCapabilityBytes
    , fromCollectionBytes
    , fromCapabilityBytes

      -- * Timestamp encoding
    , toTimestamp
    , fromTimestamp

      -- * Conversion errors
    , CodecError (..)
    , catchConversionErrors
    ) where

import Control.Exception (Handler (..), SomeException, catches, fromException, throwIO)
import Control.Monad (unless, void)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Data.Time (Day (..), UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.UUID.Types qualified as UUID
import Database.SQLite.Simple (FromRow (..), ResultError, ToRow (..), field)
import Database.SQLite.Simple.Ok (ManyErrors (..))
import Eselsohr.Article
import Eselsohr.Capability (GrantSnapshot (..), Permission (..), Scope (..), maxDelegationDepth, validateGrantStructure)
import Eselsohr.Id
import Eselsohr.WebUrl (parseWebUrl, renderWebUrl)

------------------------------------------------------------------------
-- Stored values and SQL rows
------------------------------------------------------------------------

data ArticleRow = ArticleRow
    { rowArticleId :: ByteString
    , rowTitle :: Text
    , rowUrl :: Text
    , rowState :: Text
    , rowCreatedAt :: Int64
    , rowRevision :: Int64
    }
    deriving stock (Eq)

instance FromRow ArticleRow where
    fromRow = ArticleRow <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow ArticleRow where
    toRow ArticleRow{..} = toRow (rowArticleId, rowTitle, rowUrl, rowState, rowCreatedAt, rowRevision)

data GrantRow = GrantRow
    { rowGrantId :: ByteString
    , rowTokenHash :: ByteString
    , rowParentId :: Maybe ByteString
    , rowScopeArticleId :: Maybe ByteString
    , rowExpiresAt :: Maybe Int64
    , rowDepth :: Int64
    , rowPetname :: Maybe Text
    }
    deriving stock (Eq)

instance FromRow GrantRow where
    fromRow = GrantRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow GrantRow where
    toRow GrantRow{..} = toRow (rowGrantId, rowTokenHash, rowParentId, rowScopeArticleId, rowExpiresAt, rowDepth, rowPetname)

data PermissionRow = PermissionRow
    { rowPermissionGrantId :: ByteString
    , rowPermission :: Text
    }
    deriving stock (Eq)

instance FromRow PermissionRow where
    fromRow = PermissionRow <$> field <*> field

instance ToRow PermissionRow where
    toRow PermissionRow{..} = toRow (rowPermissionGrantId, rowPermission)

data StoredArticle = StoredArticle
    { storedArticle :: Article
    , storedRevision :: Integer
    }
    deriving stock (Eq)

data StoredGrant = StoredGrant
    { storedGrant :: GrantSnapshot
    , storedTokenHash :: ByteString
    , storedPetname :: Maybe Text
    }
    deriving stock (Eq)

------------------------------------------------------------------------
-- Row conversions
------------------------------------------------------------------------

toArticleRow :: StoredArticle -> Either CodecError ArticleRow
toArticleRow StoredArticle{storedArticle = Article{..}, ..} = do
    identifier <- toUuidBytes (renderArticleId articleId)
    created <- toTimestamp articleCreatedAt
    unless (storedRevision >= 0) (Left UnrepresentableValue)
    revision <- toInt64 storedRevision

    pure (ArticleRow identifier (renderTitle articleTitle) (renderWebUrl articleUrl) (stateText articleState) created revision)

fromArticleRow :: ArticleRow -> Either CodecError StoredArticle
fromArticleRow ArticleRow{..} = do
    identifier <- fromUuidBytes parseArticleId rowArticleId
    title <- first (const InvalidValue) $ mkTitle rowTitle
    url <- first (const InvalidValue) $ parseWebUrl rowUrl
    unless (renderTitle title == rowTitle && renderWebUrl url == rowUrl && rowRevision >= 0) (Left InvalidValue)
    state <- case rowState of
        "unread" -> Right Unread
        "read" -> Right Read
        _ -> Left InvalidValue

    pure (StoredArticle (Article identifier title url state (fromTimestamp rowCreatedAt)) (toInteger rowRevision))

toGrantRows :: CollectionId -> StoredGrant -> Either CodecError (GrantRow, [PermissionRow])
toGrantRows collection StoredGrant{storedGrant = grant@GrantSnapshot{..}, ..} = do
    article <- case grantScope of
        WholeCollection owner | owner == collection -> Right Nothing
        OneArticle owner identifier | owner == collection -> Just <$> toUuidBytes (renderArticleId identifier)
        _ -> Left CollectionMismatch
    validateLocalGrant collection grant
    unless (ByteString.length storedTokenHash == 32) (Left InvalidValue)

    identifier <- toUuidBytes (renderCapabilityId grantId)
    parent <- traverse (toUuidBytes . renderCapabilityId) grantParentId
    expires <- traverse toTimestamp grantExpiresAt
    let permissions = [PermissionRow identifier (permissionText permission) | permission <- Set.toAscList grantPermissions]
    pure (GrantRow identifier storedTokenHash parent article expires (fromIntegral grantDepth) storedPetname, permissions)

fromGrantRows :: CollectionId -> GrantRow -> [PermissionRow] -> Either CodecError StoredGrant
fromGrantRows collection GrantRow{..} rows = do
    unless (ByteString.length rowTokenHash == 32 && rowDepth >= 0 && rowDepth <= fromIntegral maxDelegationDepth) (Left InvalidValue)
    identifier <- fromUuidBytes parseCapabilityId rowGrantId
    parent <- traverse (fromUuidBytes parseCapabilityId) rowParentId
    scope <- maybe (Right (WholeCollection collection)) (fmap (OneArticle collection) . fromUuidBytes parseArticleId) rowScopeArticleId

    let boundedRows = take 9 rows
    unless (length boundedRows <= 8) (Left InvalidValue)
    permissions <- traverse readPermission boundedRows

    let permissionSet = Set.fromList permissions
        grant = GrantSnapshot identifier parent scope permissionSet (fromTimestamp <$> rowExpiresAt) (fromIntegral rowDepth)
    unless (Set.size permissionSet == length permissions) (Left InvalidValue)
    validateLocalGrant collection grant
    pure (StoredGrant grant rowTokenHash rowPetname)
  where
    readPermission PermissionRow{..} = do
        unless (rowPermissionGrantId == rowGrantId) (Left InvalidValue)
        maybe (Left InvalidValue) Right (lookup rowPermission [(permissionText p, p) | p <- [minBound .. maxBound]])

validateLocalGrant :: CollectionId -> GrantSnapshot -> Either CodecError ()
validateLocalGrant collection grant@GrantSnapshot{..} = case grantParentId of
    Nothing -> void . first (const InvalidValue) $ validateGrantStructure collection grantId [grant]
    Just parent -> unless (parent /= grantId && isJust grantExpiresAt && grantDepth >= 1 && grantDepth <= maxDelegationDepth) (Left InvalidValue)

stateText :: ArticleState -> Text
stateText Unread = "unread"
stateText Read = "read"

permissionText :: Permission -> Text
permissionText = \case
    ReadArticles -> "read"
    AddArticles -> "add"
    ChangeTitles -> "title"
    ChangeReadState -> "state"
    DeleteArticles -> "delete"
    Delegate -> "delegate"
    InspectLinks -> "inspect"
    RevokeLinks -> "revoke"

------------------------------------------------------------------------
-- Identifier encoding
------------------------------------------------------------------------

toCollectionBytes :: CollectionId -> Either CodecError ByteString
toCollectionBytes = toUuidBytes . renderCollectionId

toArticleBytes :: ArticleId -> Either CodecError ByteString
toArticleBytes = toUuidBytes . renderArticleId

toCapabilityBytes :: CapabilityId -> Either CodecError ByteString
toCapabilityBytes = toUuidBytes . renderCapabilityId

fromCollectionBytes :: ByteString -> Either CodecError CollectionId
fromCollectionBytes = fromUuidBytes parseCollectionId

fromCapabilityBytes :: ByteString -> Either CodecError CapabilityId
fromCapabilityBytes = fromUuidBytes parseCapabilityId

toUuidBytes :: Text -> Either CodecError ByteString
toUuidBytes = maybe (Left InvalidValue) (Right . Lazy.toStrict . UUID.toByteString) . UUID.fromText

fromUuidBytes :: (Text -> Either IdError a) -> ByteString -> Either CodecError a
fromUuidBytes parse bytes = do
    unless (ByteString.length bytes == 16) (Left InvalidValue)
    uuid <- maybe (Left InvalidValue) Right . UUID.fromByteString $ Lazy.fromStrict bytes
    first (const InvalidValue) . parse $ UUID.toText uuid

------------------------------------------------------------------------
-- Timestamp encoding
------------------------------------------------------------------------

-- | Encode Unix microseconds, rejecting leap seconds, finer precision, and values outside Int64.
toTimestamp :: UTCTime -> Either CodecError Int64
toTimestamp (UTCTime (ModifiedJulianDay day) time) = do
    let picos = diffTimeToPicoseconds time
    unless (picos >= 0 && picos < 86400 * 10 ^ (12 :: Int) && picos `mod` 1000000 == 0) (Left UnrepresentableValue)
    toInt64 ((day - 40587) * 86400000000 + picos `div` 1000000)

fromTimestamp :: Int64 -> UTCTime
fromTimestamp micros =
    let (days, remainder) = toInteger micros `divMod` 86400000000
     in UTCTime (ModifiedJulianDay (days + 40587)) (picosecondsToDiffTime (remainder * 1000000))

toInt64 :: Integer -> Either CodecError Int64
toInt64 value
    | value < toInteger (minBound :: Int64) || value > toInteger (maxBound :: Int64) = Left UnrepresentableValue
    | otherwise = Right (fromInteger value)

------------------------------------------------------------------------
-- Conversion errors
------------------------------------------------------------------------

data CodecError = InvalidValue | UnrepresentableValue | CollectionMismatch
    deriving stock (Eq, Show)

-- | Replace row conversion errors with InvalidValue so their messages cannot expose stored values.
catchConversionErrors :: IO a -> IO (Either CodecError a)
catchConversionErrors action =
    (Right <$> action)
        `catches` [ Handler $ \(_ :: ResultError) -> pure (Left InvalidValue)
                  , Handler $ \(_ :: UnicodeException) -> pure (Left InvalidValue)
                  , Handler $ \err@(ManyErrors errors) ->
                        if not (null errors) && all isConversionError errors then pure (Left InvalidValue) else throwIO err
                  ]
  where
    isConversionError :: SomeException -> Bool
    isConversionError exception =
        isJust (fromException @ResultError exception)
            || isJust (fromException @UnicodeException exception)
            || case fromException exception of
                Just (ManyErrors errors) -> not (null errors) && all isConversionError errors
                Nothing -> False
