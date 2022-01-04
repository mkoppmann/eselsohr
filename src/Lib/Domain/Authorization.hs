module Lib.Domain.Authorization
  (
    -- * Unlock link permissions
    ViewUnlockLinksPerm
  , CreateUnlockLinksPerm
  , DeleteUnlockLinksPerm
  , ShareUnlockLinksPerm
  , canViewUnlockLinks
  , canCreateUnlockLinks
  , canDeleteUnlockLinks
  , canShareUnlockLinks

    -- * Article list permissions
  , ViewArticlesPerm
  , CreateArticlesPerm
  , ShareArticleListPerm
  , ChangeTitlesPerm
  , ChangeStatesPerm
  , DeleteArticlesPerm
  , canViewArticles
  , canCreateArticles
  , canChangeTitles
  , canChangeStates
  , canDeleteArticles
  , canShareArticleList

    -- * Article permissions
  , ViewArticlePerm
  , ChangeTitlePerm
  , ChangeStatePerm
  , DeleteArticlePerm
  , ShareArticlePerm
  , viewArticleId
  , changeTitleId
  , changeStateId
  , deleteArticleId
  , shareArticleId
  , canViewArticle
  , canChangeArticleTitle
  , canChangeArticleState
  , canDeleteArticle
  , canShareArticle

  -- * Util
  , unauthorized
  ) where

import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.Capability                                ( ArticlePerms(..)
                                                                      , ArticlesPerms(..)
                                                                      , ObjectReference(..)
                                                                      , OverviewPerms(..)
                                                                      , SharedReference(..)
                                                                      )
import           Lib.Domain.Error                                     ( AppErrorType
                                                                      , notAuthorized
                                                                      )
import           Lib.Domain.Id                                        ( Id )

type Authorized a = Either AppErrorType a

------------------------------------------------------------------------
-- Unlock link permissions
------------------------------------------------------------------------

data ViewUnlockLinksPerm = ViewUnlockLinksPerm
data CreateUnlockLinksPerm = CreateUnlockLinksPerm
data DeleteUnlockLinksPerm = DeleteUnlockLinksPerm
data ShareUnlockLinksPerm = ShareUnlockLinksPerm

canViewUnlockLinks :: ObjectReference -> Authorized ViewUnlockLinksPerm
canViewUnlockLinks (OverviewRef OverviewPerms {..}) =
  if isJust viewUnlockLinksPerm then pure ViewUnlockLinksPerm else unauthorized
canViewUnlockLinks (SharedRef SharedReference {..}) = canViewUnlockLinks sharedObjRef
canViewUnlockLinks _otherRef                        = unauthorized

canCreateUnlockLinks :: ObjectReference -> Authorized CreateUnlockLinksPerm
canCreateUnlockLinks (OverviewRef OverviewPerms {..}) =
  if isJust createUnlockLinksPerm then pure CreateUnlockLinksPerm else unauthorized
canCreateUnlockLinks (SharedRef SharedReference {..}) = canCreateUnlockLinks sharedObjRef
canCreateUnlockLinks _otherRef                        = unauthorized

canDeleteUnlockLinks :: ObjectReference -> Authorized DeleteUnlockLinksPerm
canDeleteUnlockLinks (OverviewRef OverviewPerms {..}) =
  if isJust deleteUnlockLinksPerm then pure DeleteUnlockLinksPerm else unauthorized
canDeleteUnlockLinks (SharedRef SharedReference {..}) = canDeleteUnlockLinks sharedObjRef
canDeleteUnlockLinks _otherRef                        = unauthorized

canShareUnlockLinks :: ObjectReference -> Authorized ShareUnlockLinksPerm
canShareUnlockLinks (OverviewRef OverviewPerms {..}) =
  if isJust shareUnlockLinksPerm then pure ShareUnlockLinksPerm else unauthorized
canShareUnlockLinks (SharedRef SharedReference {..}) = canShareUnlockLinks sharedObjRef
canShareUnlockLinks _otherRef                        = unauthorized

------------------------------------------------------------------------
-- Article list permissions
------------------------------------------------------------------------

data ViewArticlesPerm = ViewArticlesPerm
data CreateArticlesPerm = CreateArticlesPerm
data ChangeTitlesPerm = ChangeTitlesPerm
data ChangeStatesPerm = ChangeStatesPerm
data DeleteArticlesPerm = DeleteArticlesPerm
data ShareArticleListPerm = ShareArticleListPerm

canViewArticles :: ObjectReference -> Authorized ViewArticlesPerm
canViewArticles (ArticlesRef ArticlesPerms {..}) =
  if isJust viewArticlesPerm then pure ViewArticlesPerm else unauthorized
canViewArticles (SharedRef SharedReference {..}) = canViewArticles sharedObjRef
canViewArticles _otherRef                        = unauthorized

canCreateArticles :: ObjectReference -> Authorized CreateArticlesPerm
canCreateArticles (ArticlesRef ArticlesPerms {..}) =
  if isJust createArticlesPerm then pure CreateArticlesPerm else unauthorized
canCreateArticles (SharedRef SharedReference {..}) = canCreateArticles sharedObjRef
canCreateArticles _otherRef                        = unauthorized

canChangeTitles :: ObjectReference -> Authorized ChangeTitlesPerm
canChangeTitles (ArticlesRef ArticlesPerms {..}) =
  if isJust changeTitlesPerm then pure ChangeTitlesPerm else unauthorized
canChangeTitles (SharedRef SharedReference {..}) = canChangeTitles sharedObjRef
canChangeTitles _otherRef                        = unauthorized

canChangeStates :: ObjectReference -> Authorized ChangeStatesPerm
canChangeStates (ArticlesRef ArticlesPerms {..}) =
  if isJust changeStatesPerm then pure ChangeStatesPerm else unauthorized
canChangeStates (SharedRef SharedReference {..}) = canChangeStates sharedObjRef
canChangeStates _otherRef                        = unauthorized

canDeleteArticles :: ObjectReference -> Authorized DeleteArticlesPerm
canDeleteArticles (ArticlesRef ArticlesPerms {..}) =
  if isJust deleteArticlesPerm then pure DeleteArticlesPerm else unauthorized
canDeleteArticles (SharedRef SharedReference {..}) = canDeleteArticles sharedObjRef
canDeleteArticles _otherRef                        = unauthorized

canShareArticleList :: ObjectReference -> Authorized ShareArticleListPerm
canShareArticleList (ArticlesRef ArticlesPerms {..}) =
  if isJust shareArticleListPerm then pure ShareArticleListPerm else unauthorized
canShareArticleList (SharedRef SharedReference {..}) = canShareArticleList sharedObjRef
canShareArticleList _otherRef                        = unauthorized

------------------------------------------------------------------------
-- Article permissions
------------------------------------------------------------------------

newtype ViewArticlePerm = ViewArticlePerm (Id Article)
newtype ChangeTitlePerm = ChangeTitlePerm (Id Article)
newtype ChangeStatePerm = ChangeStatePerm (Id Article)
newtype DeleteArticlePerm = DeleteArticlePerm (Id Article)
newtype ShareArticlePerm = ShareArticlePerm (Id Article)

viewArticleId :: ViewArticlePerm -> Id Article
viewArticleId (ViewArticlePerm artId) = artId

changeTitleId :: ChangeTitlePerm -> Id Article
changeTitleId (ChangeTitlePerm artId) = artId

changeStateId :: ChangeStatePerm -> Id Article
changeStateId (ChangeStatePerm artId) = artId

deleteArticleId :: DeleteArticlePerm -> Id Article
deleteArticleId (DeleteArticlePerm artId) = artId

shareArticleId :: ShareArticlePerm -> Id Article
shareArticleId (ShareArticlePerm artId) = artId

canViewArticle :: ObjectReference -> Id Article -> Authorized ViewArticlePerm
canViewArticle (ArticlesRef ArticlesPerms {..}) artId =
  if isJust viewArticlesPerm then pure $ ViewArticlePerm artId else unauthorized
canViewArticle (ArticleRef artId ArticlePerms {..}) compareId =
  if artId == compareId && isJust viewArticlePerm then pure $ ViewArticlePerm artId else unauthorized
canViewArticle (SharedRef SharedReference {..}) compareId = canViewArticle sharedObjRef compareId
canViewArticle _otherRef                        _artId    = unauthorized

canChangeArticleTitle :: ObjectReference -> Id Article -> Authorized ChangeTitlePerm
canChangeArticleTitle (ArticlesRef ArticlesPerms {..}) artId =
  if isJust changeTitlesPerm then pure $ ChangeTitlePerm artId else unauthorized
canChangeArticleTitle (ArticleRef artId ArticlePerms {..}) compareId =
  if artId == compareId && isJust changeTitlePerm then pure $ ChangeTitlePerm artId else unauthorized
canChangeArticleTitle (SharedRef SharedReference {..}) compareId = canChangeArticleTitle sharedObjRef compareId
canChangeArticleTitle _otherRef                        _artId    = unauthorized

canChangeArticleState :: ObjectReference -> Id Article -> Authorized ChangeStatePerm
canChangeArticleState (ArticlesRef ArticlesPerms {..}) artId =
  if isJust changeStatesPerm then pure $ ChangeStatePerm artId else unauthorized
canChangeArticleState (ArticleRef artId ArticlePerms {..}) compareId =
  if artId == compareId && isJust changeStatePerm then pure $ ChangeStatePerm artId else unauthorized
canChangeArticleState (SharedRef SharedReference {..}) compareId = canChangeArticleState sharedObjRef compareId
canChangeArticleState _otherRef                        _artId    = unauthorized

canDeleteArticle :: ObjectReference -> Id Article -> Authorized DeleteArticlePerm
canDeleteArticle (ArticlesRef ArticlesPerms {..}) artId =
  if isJust deleteArticlesPerm then pure $ DeleteArticlePerm artId else unauthorized
canDeleteArticle (ArticleRef artId ArticlePerms {..}) compareId =
  if artId == compareId && isJust deleteArticlePerm then pure $ DeleteArticlePerm artId else unauthorized
canDeleteArticle (SharedRef SharedReference {..}) compareId = canDeleteArticle sharedObjRef compareId
canDeleteArticle _otherRef                        _artId    = unauthorized

canShareArticle :: ObjectReference -> Id Article -> Authorized ShareArticlePerm
canShareArticle (ArticlesRef ArticlesPerms {..}) artId =
  if isJust shareArticleListPerm then pure $ ShareArticlePerm artId else unauthorized
canShareArticle (ArticleRef artId ArticlePerms {..}) compareId =
  if artId == compareId && isJust shareArticlePerm then pure $ ShareArticlePerm artId else unauthorized
canShareArticle (SharedRef SharedReference {..}) compareId = canShareArticle sharedObjRef compareId
canShareArticle _otherRef                        _artId    = unauthorized

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

unauthorized :: Authorized a
unauthorized = Left $ notAuthorized "You are missing the required permission."
