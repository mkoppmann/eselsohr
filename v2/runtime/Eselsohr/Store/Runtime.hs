{- |
Module      : Eselsohr.Store.Runtime
Description : Store bindings and translation of SQLite results

A t'Binding' holds the canonical collections directory, one parsed bearer, and the trusted clock.
The bearer names the collection, so a binding cannot pair a bearer with another collection's file.
Only the runtime and tests construct bindings; the public library cannot depend on this private library, so application code cannot.
The clock runs inside protected transactions, so it must be trusted code: the host clock in the runtime, or a controlled clock in tests.

'resolveDirectory' resolves the collections directory once, and 'bindPresented' turns the bearer text that a request presents into a binding with the host clock.
Text that is not a well-formed bearer yields 'Nothing' before any file is opened.
These two and the opaque t'CollectionDirectory' are all the web server needs, so it never sees the bearer type, the bearer parser, or a function that opens files.

> fmap (`bindPresented` "v2.Ej5FZ-ibQtOkVkJmFBdAAA.AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8") (resolveDirectory "/srv/eselsohr/collections")

'runArticleReader' and 'runArticleWriter' handle the effects of "Eselsohr.Store" from a binding.
Each operation opens the collection the bearer names, runs one protected operation with the bound clock, and closes the connection before returning, so no connection outlives an operation.
A binding holds the bearer but no checked authority: every operation reloads the bearer's grants in a fresh transaction, so revocation and expiry apply to the next call.
The runners catch no exceptions; unexpected exceptions and cancellation propagate after the opening and transaction brackets roll back and close the connection.

> runEff . runArticleReader binding $ listArticles AllArticles

'readResult' and 'editResult' translate the result of opening a collection and running a protected operation into the application-facing results of "Eselsohr.Store".
A missing collection file and a denied bearer both become a denial, so an unknown collection cannot be told apart from an unknown secret by its result.
Every other opening or storage failure becomes a t'StoreFault', never a denial; results a read cannot produce are faults as well.
-}
module Eselsohr.Store.Runtime
    ( -- * Bindings
      Binding (..)
    , CollectionDirectory
    , resolveDirectory
    , bindPresented

      -- * Running effects
    , runArticleReader
    , runArticleWriter

      -- * Result translation
    , readResult
    , editResult
    , revised
    ) where

import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Eselsohr.Article (Article, Title)
import Eselsohr.Capability (Scope (..))
import Eselsohr.Id (ArticleId, CollectionId)
import Eselsohr.Store
import Eselsohr.Store.SQLite.Articles (StoreError (..))
import Eselsohr.Store.SQLite.Articles qualified as SQLite
import Eselsohr.Store.SQLite.Bearer (Bearer, bearerCollection, parseBearer)
import Eselsohr.Store.SQLite.Codec (StoredArticle (..))
import Eselsohr.Store.SQLite.Open (CollectionDirectory, OpenError, mkCollectionDirectory, withExistingCollection)
import Eselsohr.Store.SQLite.Open qualified as Open

------------------------------------------------------------------------
-- Bindings
------------------------------------------------------------------------

data Binding = Binding
    { bindingDirectory :: CollectionDirectory
    , bindingBearer :: Bearer
    , bindingClock :: IO UTCTime
    }

-- | Resolve an existing collections directory, or 'Nothing' when it cannot be used.
resolveDirectory :: FilePath -> IO (Maybe CollectionDirectory)
resolveDirectory = fmap (either (const Nothing) Just) . mkCollectionDirectory

-- | Bind presented bearer text with the host clock, or 'Nothing' for text that is not a well-formed bearer.
bindPresented :: CollectionDirectory -> Text -> Maybe Binding
bindPresented directory presented = case parseBearer presented of
    Left _ -> Nothing
    Right bearer -> Just (Binding directory bearer getCurrentTime)

------------------------------------------------------------------------
-- Running effects
------------------------------------------------------------------------

runArticleReader :: (IOE :> es) => Binding -> Eff (ArticleReader : es) a -> Eff es a
runArticleReader binding = interpret_ $ \case
    ListArticles selection -> liftIO (readSelection binding selection)

runArticleWriter :: (IOE :> es) => Binding -> Eff (ArticleWriter : es) a -> Eff es a
runArticleWriter binding = interpret_ $ \case
    ChangeTitle identifier title (Revision expected) -> liftIO (changeStoredTitle binding identifier title expected)

readSelection :: Binding -> ArticleSelection -> IO (Either ReadFailure [Revised Article])
readSelection (Binding directory bearer clock) selection = do
    let collection = bearerCollection bearer
    result <- withExistingCollection directory collection $ \connection ->
        SQLite.readArticles connection bearer clock (selectionScope collection selection)
    pure (readResult result)

changeStoredTitle :: Binding -> ArticleId -> Title -> Integer -> IO (Either EditFailure (Revised Article))
changeStoredTitle (Binding directory bearer clock) identifier title expected = do
    result <- withExistingCollection directory (bearerCollection bearer) $ \connection ->
        SQLite.changeTitle connection bearer clock identifier title expected
    pure (editResult result)

-- | The scope of a selection within the bound collection.
selectionScope :: CollectionId -> ArticleSelection -> Scope
selectionScope collection = \case
    AllArticles -> WholeCollection collection
    SingleArticle identifier -> OneArticle collection identifier

------------------------------------------------------------------------
-- Result translation
------------------------------------------------------------------------

-- | Translate an opening failure or the result of a protected read.
readResult :: Either OpenError (Either StoreError [StoredArticle]) -> Either ReadFailure [Revised Article]
readResult = \case
    Left err -> Left (maybe ReadDenied ReadFault (openFault err))
    Right (Left err) -> Left (readFailure err)
    Right (Right articles) -> Right (map revised articles)

-- | Translate an opening failure or the result of a protected edit.
editResult :: Either OpenError (Either StoreError StoredArticle) -> Either EditFailure (Revised Article)
editResult = \case
    Left err -> Left (maybe EditDenied EditFault (openFault err))
    Right (Left err) -> Left (editFailure err)
    Right (Right article) -> Right (revised article)

revised :: StoredArticle -> Revised Article
revised (StoredArticle article number) = Revised article (Revision number)

-- | The fault for an opening failure, or 'Nothing' for a missing collection file.
openFault :: OpenError -> Maybe StoreFault
openFault = \case
    Open.Missing -> Nothing
    Open.Busy -> Just Busy
    Open.InvalidMetadata -> Just Corrupt
    Open.Unavailable -> Just Unavailable
    Open.UnsupportedSchema -> Just Unavailable
    Open.DatabaseFault -> Just Unavailable

-- Each failure is matched explicitly, so a new StoreError needs a decision here.
readFailure :: StoreError -> ReadFailure
readFailure = \case
    StoreDenied -> ReadDenied
    StoreBusy -> ReadFault Busy
    StoreCorrupt -> ReadFault Corrupt
    StoreFault -> ReadFault Unavailable
    StoreTransactionActive -> ReadFault Unavailable
    StoreNotFound -> ReadFault Unavailable
    StoreInvalidRevision -> ReadFault Unavailable
    StoreConflict -> ReadFault Unavailable
    StoreRevisionExhausted -> ReadFault Unavailable

editFailure :: StoreError -> EditFailure
editFailure = \case
    StoreDenied -> EditDenied
    StoreNotFound -> EditNotFound
    StoreInvalidRevision -> EditInvalidRevision
    StoreConflict -> EditConflict
    StoreRevisionExhausted -> EditRevisionExhausted
    StoreBusy -> EditFault Busy
    StoreCorrupt -> EditFault Corrupt
    StoreFault -> EditFault Unavailable
    StoreTransactionActive -> EditFault Unavailable
