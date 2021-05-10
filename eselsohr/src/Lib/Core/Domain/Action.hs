module Lib.Core.Domain.Action
  ( AuthAction
  , Action(..)
  , OverviewAction(..)
  , ArticleAction(..)
  , getAction
  , getUnlockLinks
  , createUnlockLink
  , deleteUnlockLink
  , viewArticles
  , createArticle
  , viewArticle
  , changeArticleTitle
  , changeArticleState
  , deleteArticle
  , canGetUnlockLinks
  , canCreateUnlockLink
  , canDeleteUnlockLink
  , canViewArticles
  , canCreateArticle
  , canViewArticle
  , canChangeAllArticleTitles
  , canChangeArticleTitle
  , canChangeAllArticleStates
  , canChangeArticleState
  , canDeleteAllArticles
  , canDeleteArticle
  ) where

import           Lib.Core.Domain.Article        ( Article )
import           Lib.Core.Domain.Capability     ( ArticlePerms(..)
                                                , ArticlesPerms(..)
                                                , Capability
                                                , ObjectReference(..)
                                                , OverviewPerms(..)
                                                , Permission(..)
                                                )
import           Lib.Core.Domain.Id             ( Id )

newtype AuthAction = AuthAction {getAction :: Action}

data Action
  = OverviewAct !OverviewAction
  | ArticleAct !ArticleAction

data OverviewAction
  = ViewUnlockLinks
  | CreateUnlockLink !(Id Capability)
  | DeleteUnlockLink !(Id Capability)

data ArticleAction
  = ViewArticles
  | CreateArticle !(Id Article)
  | ViewArticle !(Id Article)
  | ChangeArticleTitle !(Id Article)
  | ChangeArticleState !(Id Article)
  | DeleteArticle !(Id Article)

getUnlockLinks :: ObjectReference -> Maybe AuthAction
getUnlockLinks objRef = if canGetUnlockLinks objRef
  then pure . AuthAction $ OverviewAct ViewUnlockLinks
  else Nothing

canGetUnlockLinks :: ObjectReference -> Bool
canGetUnlockLinks (OverviewRef OverviewPerms {..}) =
  isAllowed opViewUnlockLinks
canGetUnlockLinks _otherRef = False

createUnlockLink :: ObjectReference -> Id Capability -> Maybe AuthAction
createUnlockLink objRef unlockLinkId = if canCreateUnlockLink objRef
  then pure . AuthAction . OverviewAct $ CreateUnlockLink unlockLinkId
  else Nothing

canCreateUnlockLink :: ObjectReference -> Bool
canCreateUnlockLink (OverviewRef OverviewPerms {..}) =
  isAllowed opCreateUnlockLinks
canCreateUnlockLink _otherRef = False

deleteUnlockLink :: ObjectReference -> Id Capability -> Maybe AuthAction
deleteUnlockLink objRef unlockLinkId = if canDeleteUnlockLink objRef
  then pure . AuthAction . OverviewAct $ DeleteUnlockLink unlockLinkId
  else Nothing

canDeleteUnlockLink :: ObjectReference -> Bool
canDeleteUnlockLink (OverviewRef OverviewPerms {..}) =
  isAllowed opDeleteUnlockLinks
canDeleteUnlockLink _otherRef = False

viewArticles :: ObjectReference -> Maybe AuthAction
viewArticles objRef = if canViewArticles objRef
  then pure . AuthAction $ ArticleAct ViewArticles
  else Nothing

canViewArticles :: ObjectReference -> Bool
canViewArticles (ArticlesRef ArticlesPerms {..}) = isAllowed aspViewArticles
canViewArticles _otherRef                        = False

createArticle :: ObjectReference -> Id Article -> Maybe AuthAction
createArticle objRef newArtId = if canCreateArticle objRef
  then pure . AuthAction . ArticleAct $ CreateArticle newArtId
  else Nothing

canCreateArticle :: ObjectReference -> Bool
canCreateArticle (ArticlesRef ArticlesPerms {..}) = isAllowed aspCreateArticles
canCreateArticle _otherRef                        = False

viewArticle :: ObjectReference -> Id Article -> Maybe AuthAction
viewArticle objRef artId = if canViewArticle objRef artId
  then pure . AuthAction . ArticleAct $ ViewArticle artId
  else Nothing

canViewArticle :: ObjectReference -> Id Article -> Bool
canViewArticle (ArticlesRef ArticlesPerms {..}) _ = isAllowed aspViewArticles
canViewArticle (ArticleRef aId ArticlePerms {..}) compareId =
  isAllowed apViewArticle && aId == compareId
canViewArticle _otherRef _aId = False

changeArticleTitle :: ObjectReference -> Id Article -> Maybe AuthAction
changeArticleTitle objRef artId = if canChangeArticleTitle objRef artId
  then pure . AuthAction . ArticleAct $ ChangeArticleTitle artId
  else Nothing

canChangeAllArticleTitles :: ObjectReference -> Bool
canChangeAllArticleTitles (ArticlesRef ArticlesPerms {..}) =
  isAllowed aspChangeTitles
canChangeAllArticleTitles _otherRef = False

canChangeArticleTitle :: ObjectReference -> Id Article -> Bool
canChangeArticleTitle (ArticlesRef ArticlesPerms {..}) _ =
  isAllowed aspChangeTitles
canChangeArticleTitle (ArticleRef aId ArticlePerms {..}) compareId =
  isAllowed apChangeTitle && aId == compareId
canChangeArticleTitle _otherRef _aId = False

changeArticleState :: ObjectReference -> Id Article -> Maybe AuthAction
changeArticleState objRef artId = if canChangeArticleState objRef artId
  then pure . AuthAction . ArticleAct $ ChangeArticleState artId
  else Nothing

canChangeAllArticleStates :: ObjectReference -> Bool
canChangeAllArticleStates (ArticlesRef ArticlesPerms {..}) =
  isAllowed aspChangeStates
canChangeAllArticleStates _otherRef = False

canChangeArticleState :: ObjectReference -> Id Article -> Bool
canChangeArticleState (ArticlesRef ArticlesPerms {..}) _ =
  isAllowed aspChangeStates
canChangeArticleState (ArticleRef aId ArticlePerms {..}) compareId =
  isAllowed apChangeState && aId == compareId
canChangeArticleState _otherRef _aId = False

deleteArticle :: ObjectReference -> Id Article -> Maybe AuthAction
deleteArticle objRef artId = if canDeleteArticle objRef artId
  then pure . AuthAction . ArticleAct $ DeleteArticle artId
  else Nothing

canDeleteAllArticles :: ObjectReference -> Bool
canDeleteAllArticles (ArticlesRef ArticlesPerms {..}) = isAllowed aspDelete
canDeleteAllArticles _otherRef                        = False

canDeleteArticle :: ObjectReference -> Id Article -> Bool
canDeleteArticle (ArticlesRef ArticlesPerms {..}) _ = isAllowed aspDelete
canDeleteArticle (ArticleRef aId ArticlePerms {..}) compareId =
  isAllowed apDelete && aId == compareId
canDeleteArticle _otherRef _aId = False

isAllowed :: Permission -> Bool
isAllowed NotAllowed = False
isAllowed Allowed    = True
