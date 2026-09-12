{- |
Module      : Eselsohr.Store.SQLite.Internal.Transaction
Description : Read and write transaction actions and runners

t'ReadTx' and t'WriteTx' sequence SQLite actions using a fixed connection, bearer, and trusted clock.
Their types distinguish read actions from write actions and bind each action to the transaction in which it can run.

'runReadTransaction' opens a deferred transaction; 'runWriteTransaction' opens an immediate transaction with the required durability settings.
Both runners check connection settings, commit successful results, and roll back failures while preserving unexpected exceptions.
The caller must use the connection exclusively and leave its opening callback on failure so the connection closes.
-}
module Eselsohr.Store.SQLite.Internal.Transaction
    ( -- * Transaction types
      StoreError (..)
    , Tx
    , ReadTx
    , WriteTx
    , TxContext (..)

      -- * Running transactions
    , runReadTransaction
    , runWriteTransaction

      -- * Transaction actions
    , askContext
    , liftTxIO
    , checked
    , decoded
    ) where

import Control.Exception (SomeException, catch, mask, throwIO, try)
import Control.Monad (ap, join, unless)
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection (..), Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Store.SQLite.Bearer (Bearer)
import Eselsohr.Store.SQLite.Codec (CodecError, catchConversionErrors)

------------------------------------------------------------------------
-- Transaction types
------------------------------------------------------------------------

data StoreError
    = StoreDenied
    | StoreBusy
    | StoreCorrupt
    | StoreFault
    | StoreTransactionActive
    | StoreNotFound
    | StoreInvalidRevision
    | StoreConflict
    | StoreRevisionExhausted
    deriving stock (Eq, Show)

data TxContext = TxContext Connection Bearer (IO UTCTime)

data TxMode = ReadMode | WriteMode

type role Tx nominal nominal representational
newtype Tx (mode :: TxMode) tx a = Tx (TxContext -> IO (Either StoreError a))

type ReadTx tx = Tx ReadMode tx
type WriteTx tx = Tx WriteMode tx

instance Functor (Tx mode tx) where
    fmap f (Tx action) = Tx (fmap (fmap f) . action)

instance Applicative (Tx mode tx) where
    pure value = Tx (\_ -> pure (Right value))
    (<*>) = ap

instance Monad (Tx mode tx) where
    Tx action >>= next = Tx $ \context -> do
        result <- action context
        case result of
            Left err -> pure (Left err)
            Right value -> case next value of
                Tx following -> following context

------------------------------------------------------------------------
-- Running transactions
------------------------------------------------------------------------

{- | Run the action in a DEFERRED transaction, committing on success and rolling back on failure.
The connection, bearer, and trusted clock are fixed for the transaction.
The caller must use the connection exclusively and leave its opening callback on failure so the connection closes.
-}
runReadTransaction :: Connection -> Bearer -> IO UTCTime -> (forall tx. ReadTx tx a) -> IO (Either StoreError a)
runReadTransaction = runTransaction ReadMode

-- | Run the action in an IMMEDIATE transaction with the same cleanup and caller requirements as runReadTransaction.
runWriteTransaction :: Connection -> Bearer -> IO UTCTime -> (forall tx. WriteTx tx a) -> IO (Either StoreError a)
runWriteTransaction = runTransaction WriteMode

runTransaction :: TxMode -> Connection -> Bearer -> IO UTCTime -> (forall tx. Tx mode tx a) -> IO (Either StoreError a)
runTransaction mode connection bearer clock action = mask $ \restore -> do
    begun <- beginTransaction mode connection
    case begun of
        Left err -> pure (Left err)
        Right () -> do
            outcome <- try @SomeException $ do
                let context = TxContext connection bearer clock
                result <- restore . fmap join . storeCheck $ runBody mode context action
                commitOnSuccess connection result

            finishTransaction connection outcome

beginTransaction :: TxMode -> Connection -> IO (Either StoreError ())
beginTransaction mode connection = do
    ready <- join <$> storeCheck (checkReady mode connection)
    case ready of
        Left err -> pure (Left err)
        Right () -> do
            let begin = case mode of
                    ReadMode -> "BEGIN DEFERRED"
                    WriteMode -> "BEGIN IMMEDIATE"
            storeCheck (SQL.execute_ connection begin)

runBody :: TxMode -> TxContext -> Tx mode tx a -> IO (Either StoreError a)
runBody mode context@(TxContext connection _ _) (Tx perform) = do
    settings <- checkSettings mode connection
    case settings of
        Left err -> pure (Left err)
        Right () -> perform context

commitOnSuccess :: Connection -> Either StoreError a -> IO (Either StoreError a)
commitOnSuccess _ (Left err) = pure (Left err)
commitOnSuccess connection (Right value) = do
    committed <- storeCheck (SQL.execute_ connection "COMMIT")
    pure (value <$ committed)

finishTransaction :: Connection -> Either SomeException (Either StoreError a) -> IO (Either StoreError a)
finishTransaction _ (Right (Right value)) = pure (Right value)
finishTransaction connection outcome = do
    rolledBack <- rollbackIfActive connection
    case outcome of
        Left exception -> throwIO exception
        Right result -> pure (either (const (Left StoreFault)) (const result) rolledBack)

rollbackIfActive :: Connection -> IO (Either StoreError ())
rollbackIfActive connection = storeCheck $ do
    autocommit <- Direct.getAutoCommit (connectionHandle connection)
    unless autocommit (SQL.execute_ connection "ROLLBACK")

checkReady :: TxMode -> Connection -> IO (Either StoreError ())
checkReady mode connection = do
    autocommit <- Direct.getAutoCommit (connectionHandle connection)
    if not autocommit
        then pure (Left StoreTransactionActive)
        else do
            settings <- checkSettings mode connection
            case (settings, mode) of
                (Right (), WriteMode) -> configureWrite connection
                _ -> pure settings

configureWrite :: Connection -> IO (Either StoreError ())
configureWrite connection = do
    SQL.execute_ connection "PRAGMA main.synchronous = EXTRA"
    SQL.execute_ connection "PRAGMA main.secure_delete = ON"

    synchronous <- SQL.query_ connection "PRAGMA main.synchronous"
    secureDelete <- SQL.query_ connection "PRAGMA main.secure_delete"
    pure (validateWriteSettings synchronous secureDelete)

validateWriteSettings :: [Only Int64] -> [Only Int64] -> Either StoreError ()
validateWriteSettings [Only 3] [Only 1] = Right ()
validateWriteSettings _ _ = Left StoreFault

checkSettings :: TxMode -> Connection -> IO (Either StoreError ())
checkSettings mode connection = do
    foreignKeys <- SQL.query_ connection "PRAGMA foreign_keys"
    trustedSchema <- SQL.query_ connection "PRAGMA trusted_schema"
    busyTimeout <- SQL.query_ connection "PRAGMA busy_timeout"
    journal <- SQL.query_ connection "PRAGMA main.journal_mode"
    pure (validateSettings mode foreignKeys trustedSchema busyTimeout journal)

validateSettings :: TxMode -> [Only Int64] -> [Only Int64] -> [Only Int64] -> [Only Text] -> Either StoreError ()
validateSettings mode [Only 1] [Only 0] [Only 0] [Only journal]
    | journal `elem` supported = Right ()
  where
    supported = case mode of
        ReadMode -> ["delete", "truncate", "persist", "memory", "wal"]
        WriteMode -> ["delete"]
validateSettings _ _ _ _ _ = Left StoreFault

storeCheck :: IO a -> IO (Either StoreError a)
storeCheck action =
    fmap (first (const StoreCorrupt)) (catchConversionErrors action) `catch` \err ->
        pure . Left $ case SQL.sqlError err of
            SQLite.ErrorBusy -> StoreBusy
            SQLite.ErrorLocked -> StoreBusy
            _ -> StoreFault

------------------------------------------------------------------------
-- Transaction actions
------------------------------------------------------------------------

askContext :: Tx mode tx TxContext
askContext = Tx (pure . Right)

liftTxIO :: IO a -> Tx mode tx a
liftTxIO action = Tx (\_ -> Right <$> action)

checked :: Either StoreError a -> Tx mode tx a
checked result = Tx (\_ -> pure result)

decoded :: Either CodecError a -> Tx mode tx a
decoded = checked . first (const StoreCorrupt)
