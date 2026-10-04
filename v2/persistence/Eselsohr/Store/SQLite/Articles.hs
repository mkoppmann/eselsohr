{- |
Module      : Eselsohr.Store.SQLite.Articles
Description : Protected article reads, additions, and title changes

'readArticles', 'addArticle', and 'changeTitle' operate on an existing collection connection, a presented bearer, and a trusted clock.
Each operation loads current grants, samples the clock once, and checks permissions inside its transaction.
The caller supplies the clock: the runtime passes the host clock, and tests pass fixed or controlled ones.

The checks produce t'ReadRequest', t'AddRequest', and t'TitleChangeRequest' values whose private constructors retain the exact checked inputs.
Their transaction parameter prevents a request from being used in a different transaction.

'addArticle' also takes a generator of article identifiers, which it samples once inside its transaction; the runtime passes 'randomArticleId', and tests pass fixed identifiers.
It requires AddArticles on the whole collection and inserts a new unread article at revision zero, created at the instant of its authorization truncated to the whole microseconds that storage keeps.
If the collection already stores the article's canonical URL, it inserts nothing and changes nothing, and it returns the same empty result either way, so the result cannot tell an add-only holder whether the URL was new.
A generated identifier that already exists is a primary-key collision and fails the transaction.

Results include stored revisions alongside article values.
Title changes compare the submitted revision with storage; a changed title advances it once, while a matching no-op leaves it unchanged.
-}
module Eselsohr.Store.SQLite.Articles
    ( StoreError (..)

      -- * Reading articles
    , readArticles
    , ReadTx
    , ReadRequest
    , runReadTransaction
    , checkRead
    , selectArticles

      -- * Adding articles
    , addArticle
    , AddRequest
    , checkAdd
    , writeArticle
    , randomArticleId

      -- * Changing titles
    , changeTitle
    , WriteTx
    , TitleChangeRequest
    , runWriteTransaction
    , checkTitleChange
    , writeTitle
    ) where

import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Database.SQLite.Simple (Connection, Only (..))
import Database.SQLite.Simple qualified as SQL
import Eselsohr.Article (Title, articleId, newArticle, rename)
import Eselsohr.Authorization (Operation (..))
import Eselsohr.Capability (Scope (..))
import Eselsohr.Id (ArticleId)
import Eselsohr.Store.SQLite.Bearer (Bearer, bearerCollection)
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Internal.Authority (authorize, authorizeAt)
import Eselsohr.Store.SQLite.Internal.Random (randomUuidBytes)
import Eselsohr.Store.SQLite.Internal.Transaction
import Eselsohr.WebUrl (WebUrl)

------------------------------------------------------------------------
-- Reading articles
------------------------------------------------------------------------

type role ReadRequest nominal
newtype ReadRequest tx = ReadRequest Scope

readArticles :: Connection -> Bearer -> IO UTCTime -> Scope -> IO (Either StoreError [StoredArticle])
readArticles connection bearer clock scope =
    runReadTransaction connection bearer clock $ checkRead scope >>= selectArticles

checkRead :: Scope -> ReadTx tx (ReadRequest tx)
checkRead scope = do
    authorize (ReadArticlesIn scope)
    pure (ReadRequest scope)

-- | Select articles using the checked request's scope and the transaction's connection.
selectArticles :: ReadRequest tx -> ReadTx tx [StoredArticle]
selectArticles (ReadRequest scope) = do
    TxContext connection _ _ <- askContext
    rows <- case scope of
        WholeCollection _ -> liftTxIO (SQL.query_ connection "SELECT id, title, url, state, created_at, revision FROM article")
        OneArticle _ identifier -> do
            bytes <- decoded (toArticleBytes identifier)
            liftTxIO (SQL.query connection "SELECT id, title, url, state, created_at, revision FROM article WHERE id = ? LIMIT 2" (Only bytes))
    checked (decodeArticles rows)

decodeArticles :: [ArticleRow] -> Either StoreError [StoredArticle]
decodeArticles rows = do
    articles <- first (const StoreCorrupt) $ traverse fromArticleRow rows
    unless (Set.size (Set.fromList (map (articleId . storedArticle) articles)) == length articles) (Left StoreCorrupt)
    pure articles

------------------------------------------------------------------------
-- Adding articles
------------------------------------------------------------------------

type role AddRequest nominal
data AddRequest tx = AddRequest WebUrl Title UTCTime

addArticle :: Connection -> Bearer -> IO UTCTime -> IO ArticleId -> WebUrl -> Title -> IO (Either StoreError ())
addArticle connection bearer clock generate url title =
    runWriteTransaction connection bearer clock $ checkAdd url title >>= writeArticle generate

checkAdd :: WebUrl -> Title -> WriteTx tx (AddRequest tx)
checkAdd url title = do
    TxContext _ bearer _ <- askContext
    (_, instant) <- authorizeAt (AddArticle (bearerCollection bearer))
    pure (AddRequest url title instant)

-- | URL, title, and creation instant come only from this transaction's checked request.
writeArticle :: IO ArticleId -> AddRequest tx -> WriteTx tx ()
writeArticle generate (AddRequest url title instant) = do
    TxContext connection _ _ <- askContext
    identifier <- liftTxIO generate
    row <- decoded . toArticleRow $ StoredArticle (newArticle identifier title url (wholeMicroseconds instant)) 0
    liftTxIO (SQL.execute connection "INSERT INTO article (id, title, url, state, created_at, revision) VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (url) DO NOTHING" row)

-- | Drop the fraction of a microsecond that storage cannot represent.
wholeMicroseconds :: UTCTime -> UTCTime
wholeMicroseconds (UTCTime day time) = UTCTime day (picosecondsToDiffTime (diffTimeToPicoseconds time `div` 1000000 * 1000000))

-- | A fresh random UUIDv4 article identifier.
randomArticleId :: IO ArticleId
randomArticleId = do
    bytes <- randomUuidBytes
    either (const (fail "random bytes did not form a UUIDv4")) pure (fromArticleBytes bytes)

------------------------------------------------------------------------
-- Changing titles
------------------------------------------------------------------------

type role TitleChangeRequest nominal
data TitleChangeRequest tx = TitleChangeRequest ArticleId Title Integer

changeTitle :: Connection -> Bearer -> IO UTCTime -> ArticleId -> Title -> Integer -> IO (Either StoreError StoredArticle)
changeTitle connection bearer clock identifier title revision =
    runWriteTransaction connection bearer clock $ checkTitleChange identifier title revision >>= writeTitle

checkTitleChange :: ArticleId -> Title -> Integer -> WriteTx tx (TitleChangeRequest tx)
checkTitleChange identifier title revision = do
    TxContext _ bearer _ <- askContext
    authorize (RenameArticle (bearerCollection bearer) identifier)
    pure (TitleChangeRequest identifier title revision)

-- | Target, title, and expected revision come only from this transaction's checked request.
writeTitle :: TitleChangeRequest tx -> WriteTx tx StoredArticle
writeTitle (TitleChangeRequest identifier title expected) = do
    checked $ unless (expected >= 0 && expected <= maxRevision) (Left StoreInvalidRevision)

    TxContext connection _ _ <- askContext
    bytes <- decoded (toArticleBytes identifier)
    rows <- liftTxIO (SQL.query connection "SELECT id, title, url, state, created_at, revision FROM article WHERE id = ? LIMIT 2" (Only bytes))
    row <- checked $ case rows of
        [] -> Left StoreNotFound
        [value] -> Right value
        _ -> Left StoreCorrupt
    current <- decoded (fromArticleRow row)

    result <- checked (prepareTitleChange title expected current)
    when (result /= current) $ do
        updated <- decoded (toArticleRow result)
        let parameters = TitleUpdateRow (rowTitle updated) (rowRevision updated) bytes (rowRevision row)

        liftTxIO (SQL.execute connection "UPDATE article SET title = ?, revision = ? WHERE id = ? AND revision = ?" parameters)
        count <- liftTxIO (SQL.changes connection)
        checked $ unless (count == 1) (Left StoreCorrupt)

    pure result

prepareTitleChange :: Title -> Integer -> StoredArticle -> Either StoreError StoredArticle
prepareTitleChange title expected current
    | storedRevision current /= expected = Left StoreConflict
    | changed == storedArticle current = Right current
    | expected == maxRevision = Left StoreRevisionExhausted
    | otherwise = Right (StoredArticle changed (expected + 1))
  where
    changed = rename title (storedArticle current)

maxRevision :: Integer
maxRevision = toInteger (maxBound :: Int64)

data TitleUpdateRow = TitleUpdateRow Text Int64 ByteString Int64

instance SQL.ToRow TitleUpdateRow where
    toRow (TitleUpdateRow title revision identifier expected) = SQL.toRow (title, revision, identifier, expected)
