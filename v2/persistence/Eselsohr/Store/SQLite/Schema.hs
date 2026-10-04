{- |
Module      : Eselsohr.Store.SQLite.Schema
Description : Initial collection schema and initialization transactions

'initializeSchema' installs schema version 1 and the collection identity into an already-open empty database.
The tables, constraints, indexes, identity row, and version are installed in one transaction; no owner grant is created.
The indexes allow one root grant per collection and one article per stored URL, which is the canonical rendering of the article's URL.
'installSchema' performs the same installation inside a transaction that the caller already began, so collection creation can add the owner grant to the same transaction.
'checkSchemaVersion' checks whether a database has the supported version.

'withSchemaTransaction' provides the initialization transaction, committing a successful result and rolling back a failure.
It rejects an existing transaction without committing or rolling back the caller's work.
-}
module Eselsohr.Store.SQLite.Schema
    ( -- * Schema installation
      SchemaError (..)
    , initializeSchema
    , installSchema
    , checkSchemaVersion

      -- * Initialization transaction
    , withSchemaTransaction
    ) where

import Control.Exception (SomeException, catch, mask, throwIO, try)
import Control.Monad (join, unless)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (Connection (..), Only (..), SQLError (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Id (CollectionId)
import Eselsohr.Store.SQLite.Codec (catchConversionErrors, toCollectionBytes)

------------------------------------------------------------------------
-- Schema installation
------------------------------------------------------------------------

data SchemaError = TransactionActive | NoTransaction | NonEmptyDatabase | UnsupportedVersion | SchemaBusy | SchemaFault
    deriving stock (Eq, Show)

-- | Install version 1 into an existing empty database without creating an owner grant.
initializeSchema :: Connection -> CollectionId -> IO (Either SchemaError ())
initializeSchema connection = withSchemaTransaction connection . installSchema connection

{- | Install version 1 and the collection identity inside the caller's transaction, returning 'NoTransaction' without changes outside one.
SQL exceptions propagate, so the caller's transaction decides how they end it.
-}
installSchema :: Connection -> CollectionId -> IO (Either SchemaError ())
installSchema connection collection = do
    autocommit <- Direct.getAutoCommit (connectionHandle connection)
    if autocommit
        then pure (Left NoTransaction)
        else do
            version <- SQL.query_ connection "PRAGMA main.user_version" :: IO [Only Int64]
            objects <- SQL.query_ connection "SELECT name FROM main.sqlite_schema WHERE name NOT GLOB 'sqlite_*' LIMIT 1" :: IO [Only Text]
            case prepareInitialization collection version objects of
                Left err -> pure (Left err)
                Right identifier -> do
                    mapM_ (SQL.execute_ connection) schema
                    SQL.execute connection "INSERT INTO collection_meta (singleton, collection_id) VALUES (1, ?)" (Only identifier)
                    SQL.execute_ connection "PRAGMA main.user_version = 1"

                    checkSchemaVersion connection

prepareInitialization :: CollectionId -> [Only Int64] -> [Only Text] -> Either SchemaError ByteString
prepareInitialization collection versions objects = do
    unless (versions == [Only 0] && null objects) (Left NonEmptyDatabase)
    first (const SchemaFault) (toCollectionBytes collection)

checkSchemaVersion :: Connection -> IO (Either SchemaError ())
checkSchemaVersion connection = do
    result <- schemaCheck (SQL.query_ connection "PRAGMA main.user_version")
    pure (result >>= validateSchemaVersion)

validateSchemaVersion :: [Only Int64] -> Either SchemaError ()
validateSchemaVersion [Only 1] = Right ()
validateSchemaVersion _ = Left UnsupportedVersion

------------------------------------------------------------------------
-- Initialization transaction
------------------------------------------------------------------------

{- | Run schema initialization in an IMMEDIATE transaction, committing on success and rolling back on failure.
The caller must leave its opening callback on failure so the connection closes.
An existing transaction is rejected without committing or rolling it back.
-}
withSchemaTransaction :: Connection -> IO (Either SchemaError a) -> IO (Either SchemaError a)
withSchemaTransaction connection action = mask $ \restore -> do
    begun <- beginTransaction connection
    case begun of
        Left err -> pure (Left err)
        Right () -> do
            outcome <- try @SomeException $ do
                result <- restore (join <$> schemaCheck action)
                commitOnSuccess connection result

            finishTransaction connection outcome

beginTransaction :: Connection -> IO (Either SchemaError ())
beginTransaction connection = do
    ready <- join <$> schemaCheck (checkReady connection)
    case ready of
        Left err -> pure (Left err)
        Right () -> schemaCheck (SQL.execute_ connection "BEGIN IMMEDIATE")

checkReady :: Connection -> IO (Either SchemaError ())
checkReady connection = do
    autocommit <- Direct.getAutoCommit (connectionHandle connection)
    if not autocommit
        then pure (Left TransactionActive)
        else do
            foreignKeys <- SQL.query_ connection "PRAGMA foreign_keys"
            journal <- SQL.query_ connection "PRAGMA main.journal_mode"
            pure (validateSettings foreignKeys journal)

validateSettings :: [Only Int64] -> [Only Text] -> Either SchemaError ()
validateSettings [Only 1] [Only journal]
    | journal `elem` ["delete", "truncate", "persist", "memory", "wal"] = Right ()
validateSettings _ _ = Left SchemaFault

commitOnSuccess :: Connection -> Either SchemaError a -> IO (Either SchemaError a)
commitOnSuccess _ (Left err) = pure (Left err)
commitOnSuccess connection (Right value) = do
    committed <- schemaCheck (SQL.execute_ connection "COMMIT")
    pure (value <$ committed)

finishTransaction :: Connection -> Either SomeException (Either SchemaError a) -> IO (Either SchemaError a)
finishTransaction _ (Right (Right value)) = pure (Right value)
finishTransaction connection outcome = do
    rolledBack <- rollbackIfActive connection
    case outcome of
        Left exception -> throwIO exception
        Right result -> pure (either (const (Left SchemaFault)) (const result) rolledBack)

rollbackIfActive :: Connection -> IO (Either SchemaError ())
rollbackIfActive connection = schemaCheck $ do
    autocommit <- Direct.getAutoCommit (connectionHandle connection)
    unless autocommit (SQL.execute_ connection "ROLLBACK")

schemaCheck :: IO a -> IO (Either SchemaError a)
schemaCheck action =
    fmap (either (const (Left SchemaFault)) Right) (catchConversionErrors action) `catch` \err ->
        pure . Left $ case sqlError err of
            SQLite.ErrorBusy -> SchemaBusy
            SQLite.ErrorLocked -> SchemaBusy
            _ -> SchemaFault

------------------------------------------------------------------------
-- SQL schema
------------------------------------------------------------------------

schema :: [SQL.Query]
schema =
    [ "CREATE TABLE collection_meta (singleton INTEGER PRIMARY KEY CHECK (singleton = 1), collection_id BLOB NOT NULL UNIQUE CHECK (length(collection_id) = 16)) STRICT"
    , "CREATE TABLE article (id BLOB PRIMARY KEY CHECK (length(id) = 16), title TEXT NOT NULL CHECK (length(title) > 0), url TEXT NOT NULL CHECK (length(url) > 0), state TEXT NOT NULL CHECK (state IN ('unread', 'read')), created_at INTEGER NOT NULL, revision INTEGER NOT NULL DEFAULT 0 CHECK (revision >= 0)) STRICT"
    , "CREATE TABLE capability (id BLOB PRIMARY KEY CHECK (length(id) = 16), token_hash BLOB NOT NULL UNIQUE CHECK (length(token_hash) = 32), parent_id BLOB REFERENCES capability(id) ON DELETE CASCADE, scope_article_id BLOB REFERENCES article(id) ON DELETE CASCADE, expires_at INTEGER, depth INTEGER NOT NULL CHECK (depth BETWEEN 0 AND 4), petname TEXT, CHECK (parent_id IS NULL OR parent_id <> id), CHECK ((parent_id IS NULL AND depth = 0 AND expires_at IS NULL AND scope_article_id IS NULL) OR (parent_id IS NOT NULL AND depth > 0 AND expires_at IS NOT NULL))) STRICT"
    , "CREATE UNIQUE INDEX one_root ON capability((1)) WHERE parent_id IS NULL"
    , "CREATE UNIQUE INDEX article_url ON article(url)"
    , "CREATE TABLE capability_permission (capability_id BLOB NOT NULL REFERENCES capability(id) ON DELETE CASCADE, permission TEXT NOT NULL CHECK (permission IN ('read', 'add', 'title', 'state', 'delete', 'delegate', 'inspect', 'revoke')), PRIMARY KEY (capability_id, permission)) STRICT"
    ]
