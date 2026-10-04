{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Eselsohr.Store
Description : Article read and write effects for one presented capability

t'ArticleReader' and t'ArticleWriter' are the effects through which application code reads and changes articles.
Each operation is one complete protected operation: its handler is bound to one presented bearer, authenticates it, checks current authority, and runs the operation in one transaction.
Application code names no collection, connection, path, or clock, and this module provides no handler.
Using an operation grants no authority, because the bound bearer determines what each operation may do.
Code run with only t'ArticleReader' handled cannot change articles, whatever its bearer permits.

'listArticles' selects all articles or one article of the bound collection; an authorized selection of a missing article returns an empty list.
'changeTitle' takes the t'Revision' the caller last read and returns the stored article with its resulting revision.
Results pair each article with its stored revision as t'Revised', outside the article value.
A submitted revision is untrusted concurrency metadata: it is compared with storage only after authorization, and any mismatch is a conflict, even for an unchanged title.

> retitle :: (ArticleReader :> es, ArticleWriter :> es) => ArticleId -> Title -> Eff es (Maybe (Either EditFailure (Revised Article)))
> retitle identifier title = do
>     selected <- listArticles (SingleArticle identifier)
>     case selected of
>         Right [Revised _ current] -> Just <$> changeTitle identifier title current
>         _ -> pure Nothing

Expected failures are t'ReadFailure' for reads and t'EditFailure' for revision-checked edits.
A t'StoreFault' reports contention, corruption, or unavailable storage and is never a denial.
No failure value contains submitted or stored data.
-}
module Eselsohr.Store
    ( -- * Revisions
      Revision (..)
    , Revised (..)

      -- * Failures
    , StoreFault (..)
    , ReadFailure (..)
    , EditFailure (..)

      -- * Reading articles
    , ArticleReader (..)
    , ArticleSelection (..)
    , listArticles

      -- * Writing articles
    , ArticleWriter (..)
    , changeTitle
    ) where

import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Eselsohr.Article (Article, Title)
import Eselsohr.Id (ArticleId)

------------------------------------------------------------------------
-- Revisions
------------------------------------------------------------------------

-- | A stored revision as read from the Store; the Store checks its range after authorization.
newtype Revision = Revision Integer
    deriving stock (Eq, Ord, Show)

-- | A stored value with the revision it was read or written at.
data Revised a = Revised
    { revisedValue :: a
    , revision :: Revision
    }
    deriving stock (Eq, Show)

------------------------------------------------------------------------
-- Failures
------------------------------------------------------------------------

-- | Storage failures: 'Busy' is retryable contention, and the others need the operator.
data StoreFault = Busy | Corrupt | Unavailable
    deriving stock (Eq, Show)

data ReadFailure = ReadDenied | ReadFault StoreFault
    deriving stock (Eq, Show)

data EditFailure
    = EditDenied
    | EditNotFound
    | EditConflict
    | EditInvalidRevision
    | EditRevisionExhausted
    | EditFault StoreFault
    deriving stock (Eq, Show)

------------------------------------------------------------------------
-- Reading articles
------------------------------------------------------------------------

data ArticleReader :: Effect where
    ListArticles :: ArticleSelection -> ArticleReader m (Either ReadFailure [Revised Article])

type instance DispatchOf ArticleReader = Dynamic

-- | All articles of the bound collection, or one article identifier in it.
data ArticleSelection = AllArticles | SingleArticle ArticleId
    deriving stock (Eq, Show)

-- | Read the selected articles with their stored revisions.
listArticles :: (ArticleReader :> es) => ArticleSelection -> Eff es (Either ReadFailure [Revised Article])
listArticles = send . ListArticles

------------------------------------------------------------------------
-- Writing articles
------------------------------------------------------------------------

data ArticleWriter :: Effect where
    ChangeTitle :: ArticleId -> Title -> Revision -> ArticleWriter m (Either EditFailure (Revised Article))

type instance DispatchOf ArticleWriter = Dynamic

-- | Change the title of an article read at the expected revision.
changeTitle :: (ArticleWriter :> es) => ArticleId -> Title -> Revision -> Eff es (Either EditFailure (Revised Article))
changeTitle identifier title expected = send (ChangeTitle identifier title expected)
