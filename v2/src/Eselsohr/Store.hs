{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Eselsohr.Store
Description : Article read and write effects for one presented capability

t'ArticleReader' and t'ArticleWriter' are the effects through which application code reads and changes articles, and t'GrantReader' is the effect through which it learns what the bound grant permits.
Each operation is one complete protected operation: its handler is bound to one presented bearer, authenticates it, checks current authority, and runs the operation in one transaction.
Application code names no collection, connection, path, or clock, and this module provides no handler.
Using an operation grants no authority, because the bound bearer determines what each operation may do.
Code run with only t'ArticleReader' and t'GrantReader' handled cannot change articles, whatever its bearer permits.

'listArticles' selects all articles or one article of the bound collection; an authorized selection of a missing article returns an empty list.
'addArticle' adds a validated URL and title to the bound collection and returns nothing, so an add-only holder learns neither whether the URL was already stored nor anything else about the collection.
'changeTitle' takes the t'Revision' the caller last read and returns the stored article with its resulting revision.
Results pair each article with its stored revision as t'Revised', outside the article value.
'describeGrant' returns a t'GrantView' of what the bound grant permits: its permissions, its scope, and its expiration, if any, and nothing about its identity, ancestors, or issuer.
It requires no permission, only a valid grant, so a holder can learn which forms to offer before attempting an operation.
A submitted revision is untrusted concurrency metadata: it is compared with storage only after authorization, and any mismatch is a conflict, even for an unchanged title.

> retitle :: (ArticleReader :> es, ArticleWriter :> es) => ArticleId -> Title -> Eff es (Maybe (Either EditFailure (Revised Article)))
> retitle identifier title = do
>     selected <- listArticles (SingleArticle identifier)
>     case selected of
>         Right [Revised _ current] -> Just <$> changeTitle identifier title current
>         _ -> pure Nothing

Expected failures are t'ReadFailure' for reads and descriptions, and t'EditFailure' for additions and revision-checked edits; an addition fails only with 'EditDenied' or 'EditFault'.
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

      -- * Describing grants
    , GrantReader (..)
    , GrantView (..)
    , describeGrant

      -- * Reading articles
    , ArticleReader (..)
    , ArticleSelection (..)
    , listArticles

      -- * Writing articles
    , ArticleWriter (..)
    , addArticle
    , changeTitle
    ) where

import Data.Time (UTCTime)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Eselsohr.Article (Article, Title)
import Eselsohr.Capability (Permissions, Scope)
import Eselsohr.Id (ArticleId)
import Eselsohr.WebUrl (WebUrl)

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
-- Describing grants
------------------------------------------------------------------------

data GrantReader :: Effect where
    DescribeGrant :: GrantReader m (Either ReadFailure GrantView)

type instance DispatchOf GrantReader = Dynamic

-- | What a presented grant permits, without its identifier, parent, depth, or petname.
data GrantView = GrantView
    { viewPermissions :: Permissions
    , viewScope :: Scope
    , viewExpiresAt :: Maybe UTCTime
    }
    deriving stock (Eq, Show)

-- | Describe the bound grant.
describeGrant :: (GrantReader :> es) => Eff es (Either ReadFailure GrantView)
describeGrant = send DescribeGrant

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
    AddArticle :: WebUrl -> Title -> ArticleWriter m (Either EditFailure ())
    ChangeTitle :: ArticleId -> Title -> Revision -> ArticleWriter m (Either EditFailure (Revised Article))

type instance DispatchOf ArticleWriter = Dynamic

-- | Add an article with a URL and title, unless the collection already stores the URL.
addArticle :: (ArticleWriter :> es) => WebUrl -> Title -> Eff es (Either EditFailure ())
addArticle url title = send (AddArticle url title)

-- | Change the title of an article read at the expected revision.
changeTitle :: (ArticleWriter :> es) => ArticleId -> Title -> Revision -> Eff es (Either EditFailure (Revised Article))
changeTitle identifier title expected = send (ChangeTitle identifier title expected)
