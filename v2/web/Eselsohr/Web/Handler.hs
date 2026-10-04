{- |
Module      : Eselsohr.Web.Handler
Description : Application functions behind the web routes

'showPage' gathers what the page of the bound bearer needs: the description of its grant and, when the grant has ReadArticles, the articles of the grant's scope.
Its type names only the grant and article read effects, so it performs no other IO and cannot change articles; the web entry runs it with effects bound to the presented bearer.

A denial from either call makes the outcome 'PageDenied', so an expired, revoked, or unknown bearer looks like any invalid link.
A revocation between the two calls also ends in 'PageDenied'.

'submitArticle' adds a validated URL and title through the bound article writer and needs only that effect.
Its outcome says only whether the submission was accepted, denied, or failed, the same for a new and an already stored URL.
-}
module Eselsohr.Web.Handler
    ( PageOutcome (..)
    , showPage
    , SubmitOutcome (..)
    , submitArticle
    ) where

import Data.Set qualified as Set
import Effectful (Eff, (:>))
import Eselsohr.Article (Article, Title)
import Eselsohr.Capability (Permission (..), Scope (..))
import Eselsohr.Store
import Eselsohr.WebUrl (WebUrl)

data PageOutcome
    = PageShown GrantView (Maybe [Revised Article])
    | PageDenied
    | PageFault StoreFault

showPage :: (GrantReader :> es, ArticleReader :> es) => Eff es PageOutcome
showPage = do
    described <- describeGrant
    case described of
        Left failure -> pure (readOutcome failure)
        Right view
            | ReadArticles `Set.member` viewPermissions view -> do
                listed <- listArticles (selectionFor (viewScope view))
                pure $ either readOutcome (PageShown view . Just) listed
            | otherwise -> pure (PageShown view Nothing)

-- | The articles a scope covers: the whole collection, or the one article.
selectionFor :: Scope -> ArticleSelection
selectionFor = \case
    WholeCollection _ -> AllArticles
    OneArticle _ identifier -> SingleArticle identifier

readOutcome :: ReadFailure -> PageOutcome
readOutcome = \case
    ReadDenied -> PageDenied
    ReadFault fault -> PageFault fault

data SubmitOutcome = Submitted | SubmitDenied | SubmitFault StoreFault

submitArticle :: (ArticleWriter :> es) => WebUrl -> Title -> Eff es SubmitOutcome
submitArticle url title = either submitFailure (const Submitted) <$> addArticle url title

-- An addition fails only with a denial or a fault; the other edit failures cannot occur and count as faults.
submitFailure :: EditFailure -> SubmitOutcome
submitFailure = \case
    EditDenied -> SubmitDenied
    EditFault fault -> SubmitFault fault
    EditNotFound -> SubmitFault Unavailable
    EditConflict -> SubmitFault Unavailable
    EditInvalidRevision -> SubmitFault Unavailable
    EditRevisionExhausted -> SubmitFault Unavailable
