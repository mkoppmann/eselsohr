{- |
Module      : Eselsohr.Store.SQLite.Articles
Description : Protected article reads and title changes

'readArticles' and 'changeTitle' operate on an existing collection connection and a presented bearer.
Each operation loads current grants and checks permissions inside its transaction.

The checks produce t'ReadRequest' and t'TitleChangeRequest' values whose private constructors retain the exact checked inputs.
Their transaction parameter prevents a request from being used in a different transaction.

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
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Connection, Only (..))
import Database.SQLite.Simple qualified as SQL
import Eselsohr.Article (Title, articleId, rename)
import Eselsohr.Authorization (Operation (..))
import Eselsohr.Capability (Scope (..))
import Eselsohr.Id (ArticleId)
import Eselsohr.Store.SQLite.Bearer (Bearer, bearerCollection)
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Internal.Authority (authorize)
import Eselsohr.Store.SQLite.Internal.Transaction

------------------------------------------------------------------------
-- Reading articles
------------------------------------------------------------------------

type role ReadRequest nominal
newtype ReadRequest tx = ReadRequest Scope

readArticles :: Connection -> Bearer -> Scope -> IO (Either StoreError [StoredArticle])
readArticles connection bearer scope =
    runReadTransaction connection bearer getCurrentTime $ checkRead scope >>= selectArticles

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
-- Changing titles
------------------------------------------------------------------------

type role TitleChangeRequest nominal
data TitleChangeRequest tx = TitleChangeRequest ArticleId Title Integer

changeTitle :: Connection -> Bearer -> ArticleId -> Title -> Integer -> IO (Either StoreError StoredArticle)
changeTitle connection bearer identifier title revision =
    runWriteTransaction connection bearer getCurrentTime $ checkTitleChange identifier title revision >>= writeTitle

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
