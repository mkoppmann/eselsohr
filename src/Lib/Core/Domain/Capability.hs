module Lib.Core.Domain.Capability
  (
  -- * Capability related
    Capability(..)
  , ObjectReference
  , SharedReference(..)
  , OverviewPerms(..)
  , ArticlesPerms(..)
  , ArticlePerms(..)
  , Permissions(..)
  , Permission
  , maybePerm
  , defaultOverviewRef
  , createSharedOverviewRef
  , defaultArticlesRef
  , createSharedArticlesRef
  , defaultArticleRef
  , createSharedArticleRef
  -- * Action related
  , AuthAction(getAction)
  , Action(..)
  , OverviewAction(..)
  , ArticleAction(..)
  , SharedRefAction(..)
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
  , viewSharedRefs
  , createSharedRef
  , deleteSharedRef
  , canManageSharedRef
  , isSharedRef
  , isSharedOverviewRef
  , isSharedArticlesRef
  , isSharedArticleRef
  ) where

import           Codec.Serialise.Class          ( Serialise )
import           Lib.Core.Domain.Article        ( Article )
import           Lib.Core.Domain.ExpirationDate ( ExpirationDate )
import           Lib.Core.Domain.Id             ( Id )
import           Web.HttpApiData                ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                , parseBoundedUrlPiece
                                                , showTextData
                                                )

data Capability = Capability
  { objectRef         :: !ObjectReference
  , petname           :: !(Maybe Text)
  , capExpirationDate :: !(Maybe ExpirationDate)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

instance Ord Capability where
  compare a b = compare (capExpirationDate a) (capExpirationDate b)

data ObjectReference
  = OverviewRef !OverviewPerms
  | ArticlesRef !ArticlesPerms
  | ArticleRef !(Id Article) !ArticlePerms
  | SharedRef !SharedReference
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

newtype SharedReference = SharedReference
  { sharedObjRef :: ObjectReference
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data Permissions
  = OverviewPermissions !OverviewPerms
  | ArticlesPermissions !ArticlesPerms
  | ArticlePermissions !ArticlePerms

data OverviewPerms = OverviewPerms
  { opViewUnlockLinks   :: !Permission
  , opCreateUnlockLinks :: !Permission
  , opDeleteUnlockLinks :: !Permission
  , opShareLinks        :: !Permission
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data ArticlesPerms = ArticlesPerms
  { aspViewArticles   :: !Permission
  , aspCreateArticles :: !Permission
  , aspChangeTitles   :: !Permission
  , aspChangeStates   :: !Permission
  , aspDelete         :: !Permission
  , aspShareLinks     :: !Permission
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data ArticlePerms = ArticlePerms
  { apViewArticle :: !Permission
  , apChangeTitle :: !Permission
  , apChangeState :: !Permission
  , apDelete      :: !Permission
  , apShareLinks  :: !Permission
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data Permission = NotAllowed | Allowed
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass Serialise

instance FromHttpApiData Permission where
  parseUrlPiece = parseBoundedUrlPiece

instance ToHttpApiData Permission where
  toUrlPiece = showTextData

newtype AuthAction = AuthAction {getAction :: Action}

data Action
  = OverviewAct !OverviewAction
  | ArticleAct !ArticleAction
  | SharedRefAct !SharedRefAction

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

data SharedRefAction
  = ViewSharedRefs
  | CreateSharedRef !(Id Capability)
  | DeleteSharedRef !(Id Capability)

defaultOverviewRef :: ObjectReference
defaultOverviewRef = OverviewRef defaultOverviewPerms

defaultOverviewPerms :: OverviewPerms
defaultOverviewPerms = OverviewPerms Allowed Allowed Allowed Allowed

createSharedOverviewRef
  :: ObjectReference -> OverviewPerms -> Maybe ObjectReference
createSharedOverviewRef (OverviewRef ogPerms) newPerms =
  let (OverviewPerms ogP1  ogP2  ogP3  ogP4 ) = ogPerms
      (OverviewPerms newP1 newP2 newP3 newP4) = newPerms
      validPermissions                        = all
        (uncurry (>=))
        [(ogP1, newP1), (ogP2, newP2), (ogP3, newP3), (ogP4, newP4)]
  in  if validPermissions
        then pure . SharedRef $ SharedReference (OverviewRef newPerms)
        else Nothing
createSharedOverviewRef _otherRef _newPerms = Nothing

defaultArticlesRef :: ObjectReference
defaultArticlesRef = ArticlesRef defaultArticlesPermissions

defaultArticlesPermissions :: ArticlesPerms
defaultArticlesPermissions =
  ArticlesPerms Allowed Allowed Allowed Allowed Allowed Allowed

createSharedArticlesRef
  :: ObjectReference -> ArticlesPerms -> Maybe ObjectReference
createSharedArticlesRef (ArticlesRef ogPerms) newPerms =
  let (ArticlesPerms ogP1  ogP2  ogP3  ogP4  ogP5  ogP6 ) = ogPerms
      (ArticlesPerms newP1 newP2 newP3 newP4 newP5 newP6) = newPerms
      validPermissions = all
        (uncurry (>=))
        [ (ogP1, newP1)
        , (ogP2, newP2)
        , (ogP3, newP3)
        , (ogP4, newP4)
        , (ogP5, newP5)
        , (ogP6, newP6)
        ]
  in  if validPermissions
        then pure . SharedRef $ SharedReference (ArticlesRef newPerms)
        else Nothing
createSharedArticlesRef _otherRef _newPerms = Nothing

defaultArticleRef :: Id Article -> ObjectReference
defaultArticleRef artId = ArticleRef artId defaultArticlePermissions

defaultArticlePermissions :: ArticlePerms
defaultArticlePermissions =
  ArticlePerms Allowed Allowed Allowed Allowed Allowed

createSharedArticleRef
  :: ObjectReference -> ArticlePerms -> Id Article -> Maybe ObjectReference
createSharedArticleRef (ArticlesRef ogPerms) newPerms articleId =
  let ArticlesPerms {..} = ogPerms
      ArticlePerms {..}  = newPerms
      validPermissions   = all
        (uncurry (>=))
        [ (aspViewArticles, apViewArticle)
        , (aspChangeTitles, apChangeTitle)
        , (aspChangeTitles, apChangeState)
        , (aspDelete      , apDelete)
        , (aspShareLinks  , apShareLinks)
        ]
  in  if validPermissions
        then pure . SharedRef $ SharedReference (ArticleRef articleId newPerms)
        else Nothing
createSharedArticleRef (ArticleRef artId ogPerms) newPerms compareId =
  let (ArticlePerms ogP1  ogP2  ogP3  ogP4  ogP5 ) = ogPerms
      (ArticlePerms newP1 newP2 newP3 newP4 newP5) = newPerms
      validPermissions                             = (artId == compareId) && all
        (uncurry (>=))
        [ (ogP1, newP1)
        , (ogP2, newP2)
        , (ogP3, newP3)
        , (ogP4, newP4)
        , (ogP5, newP5)
        ]
  in  if validPermissions
        then pure . SharedRef $ SharedReference (ArticleRef artId newPerms)
        else Nothing
createSharedArticleRef _otherRef _newPerms _compareId = Nothing

getUnlockLinks :: ObjectReference -> Maybe AuthAction
getUnlockLinks objRef = if canGetUnlockLinks objRef
  then pure . AuthAction $ OverviewAct ViewUnlockLinks
  else Nothing

canGetUnlockLinks :: ObjectReference -> Bool
canGetUnlockLinks (OverviewRef OverviewPerms {..}) =
  isAllowed opViewUnlockLinks
canGetUnlockLinks (SharedRef SharedReference {..}) =
  canGetUnlockLinks sharedObjRef
canGetUnlockLinks _otherRef = False

createUnlockLink :: ObjectReference -> Id Capability -> Maybe AuthAction
createUnlockLink objRef unlockLinkId = if canCreateUnlockLink objRef
  then pure . AuthAction . OverviewAct $ CreateUnlockLink unlockLinkId
  else Nothing

canCreateUnlockLink :: ObjectReference -> Bool
canCreateUnlockLink (OverviewRef OverviewPerms {..}) =
  isAllowed opCreateUnlockLinks
canCreateUnlockLink (SharedRef SharedReference {..}) =
  canCreateUnlockLink sharedObjRef
canCreateUnlockLink _otherRef = False

deleteUnlockLink :: ObjectReference -> Id Capability -> Maybe AuthAction
deleteUnlockLink objRef unlockLinkId = if canDeleteUnlockLink objRef
  then pure . AuthAction . OverviewAct $ DeleteUnlockLink unlockLinkId
  else Nothing

canDeleteUnlockLink :: ObjectReference -> Bool
canDeleteUnlockLink (OverviewRef OverviewPerms {..}) =
  isAllowed opDeleteUnlockLinks
canDeleteUnlockLink (SharedRef SharedReference {..}) =
  canDeleteUnlockLink sharedObjRef
canDeleteUnlockLink _otherRef = False

viewArticles :: ObjectReference -> Maybe AuthAction
viewArticles objRef = if canViewArticles objRef
  then pure . AuthAction $ ArticleAct ViewArticles
  else Nothing

canViewArticles :: ObjectReference -> Bool
canViewArticles (ArticlesRef ArticlesPerms {..}) = isAllowed aspViewArticles
canViewArticles (SharedRef SharedReference {..}) = canViewArticles sharedObjRef
canViewArticles _otherRef = False

createArticle :: ObjectReference -> Id Article -> Maybe AuthAction
createArticle objRef newArtId = if canCreateArticle objRef
  then pure . AuthAction . ArticleAct $ CreateArticle newArtId
  else Nothing

canCreateArticle :: ObjectReference -> Bool
canCreateArticle (ArticlesRef ArticlesPerms {..}) = isAllowed aspCreateArticles
canCreateArticle (SharedRef SharedReference {..}) =
  canCreateArticle sharedObjRef
canCreateArticle _otherRef = False

viewArticle :: ObjectReference -> Id Article -> Maybe AuthAction
viewArticle objRef artId = if canViewArticle objRef artId
  then pure . AuthAction . ArticleAct $ ViewArticle artId
  else Nothing

canViewArticle :: ObjectReference -> Id Article -> Bool
canViewArticle (ArticlesRef ArticlesPerms {..}) _ = isAllowed aspViewArticles
canViewArticle (ArticleRef aId ArticlePerms {..}) compareId =
  isAllowed apViewArticle && aId == compareId
canViewArticle (SharedRef SharedReference {..}) compareId =
  canViewArticle sharedObjRef compareId
canViewArticle _otherRef _aId = False

changeArticleTitle :: ObjectReference -> Id Article -> Maybe AuthAction
changeArticleTitle objRef artId = if canChangeArticleTitle objRef artId
  then pure . AuthAction . ArticleAct $ ChangeArticleTitle artId
  else Nothing

canChangeAllArticleTitles :: ObjectReference -> Bool
canChangeAllArticleTitles (ArticlesRef ArticlesPerms {..}) =
  isAllowed aspChangeTitles
canChangeAllArticleTitles (SharedRef SharedReference {..}) =
  canChangeAllArticleTitles sharedObjRef
canChangeAllArticleTitles _otherRef = False

canChangeArticleTitle :: ObjectReference -> Id Article -> Bool
canChangeArticleTitle (ArticlesRef ArticlesPerms {..}) _ =
  isAllowed aspChangeTitles
canChangeArticleTitle (ArticleRef aId ArticlePerms {..}) compareId =
  isAllowed apChangeTitle && aId == compareId
canChangeArticleTitle (SharedRef SharedReference {..}) compareId =
  canChangeArticleTitle sharedObjRef compareId
canChangeArticleTitle _otherRef _aId = False

changeArticleState :: ObjectReference -> Id Article -> Maybe AuthAction
changeArticleState objRef artId = if canChangeArticleState objRef artId
  then pure . AuthAction . ArticleAct $ ChangeArticleState artId
  else Nothing

canChangeAllArticleStates :: ObjectReference -> Bool
canChangeAllArticleStates (ArticlesRef ArticlesPerms {..}) =
  isAllowed aspChangeStates
canChangeAllArticleStates (SharedRef SharedReference {..}) =
  canChangeAllArticleStates sharedObjRef
canChangeAllArticleStates _otherRef = False

canChangeArticleState :: ObjectReference -> Id Article -> Bool
canChangeArticleState (ArticlesRef ArticlesPerms {..}) _ =
  isAllowed aspChangeStates
canChangeArticleState (ArticleRef aId ArticlePerms {..}) compareId =
  isAllowed apChangeState && aId == compareId
canChangeArticleState (SharedRef SharedReference {..}) compareId =
  canChangeArticleState sharedObjRef compareId
canChangeArticleState _otherRef _aId = False

deleteArticle :: ObjectReference -> Id Article -> Maybe AuthAction
deleteArticle objRef artId = if canDeleteArticle objRef artId
  then pure . AuthAction . ArticleAct $ DeleteArticle artId
  else Nothing

canDeleteAllArticles :: ObjectReference -> Bool
canDeleteAllArticles (ArticlesRef ArticlesPerms {..}) = isAllowed aspDelete
canDeleteAllArticles (SharedRef SharedReference {..}) =
  canDeleteAllArticles sharedObjRef
canDeleteAllArticles _otherRef = False

canDeleteArticle :: ObjectReference -> Id Article -> Bool
canDeleteArticle (ArticlesRef ArticlesPerms {..}) _ = isAllowed aspDelete
canDeleteArticle (ArticleRef aId ArticlePerms {..}) compareId =
  isAllowed apDelete && aId == compareId
canDeleteArticle (SharedRef SharedReference {..}) compareId =
  canDeleteArticle sharedObjRef compareId
canDeleteArticle _otherRef _aId = False

viewSharedRefs :: ObjectReference -> Maybe AuthAction
viewSharedRefs objRef = if canManageSharedRef objRef
  then pure . AuthAction $ SharedRefAct ViewSharedRefs
  else Nothing

createSharedRef :: ObjectReference -> Id Capability -> Maybe AuthAction
createSharedRef objRef newSharedRefId = if canManageSharedRef objRef
  then pure . AuthAction . SharedRefAct $ CreateSharedRef newSharedRefId
  else Nothing

deleteSharedRef :: ObjectReference -> Id Capability -> Maybe AuthAction
deleteSharedRef objRef sharedRefId = if canManageSharedRef objRef
  then pure . AuthAction . SharedRefAct $ DeleteSharedRef sharedRefId
  else Nothing

canManageSharedRef :: ObjectReference -> Bool
canManageSharedRef (OverviewRef OverviewPerms {..}   ) = isAllowed opShareLinks
canManageSharedRef (ArticlesRef ArticlesPerms {..}   ) = isAllowed aspShareLinks
canManageSharedRef (ArticleRef _aId ArticlePerms {..}) = isAllowed apShareLinks
canManageSharedRef (SharedRef SharedReference {..}) =
  canManageSharedRef sharedObjRef

isSharedRef :: ObjectReference -> Bool
isSharedRef (SharedRef _) = True
isSharedRef _otherRef     = False

isSharedOverviewRef :: ObjectReference -> Bool
isSharedOverviewRef (SharedRef SharedReference {..}) = case sharedObjRef of
  OverviewRef _ -> True
  _otherRef     -> False
isSharedOverviewRef _otherRef = False

isSharedArticlesRef :: ObjectReference -> Bool
isSharedArticlesRef (SharedRef SharedReference {..}) = case sharedObjRef of
  ArticlesRef _ -> True
  _otherRef     -> False
isSharedArticlesRef _otherRef = False

isSharedArticleRef :: ObjectReference -> Bool
isSharedArticleRef (SharedRef SharedReference {..}) = case sharedObjRef of
  ArticleRef _ _ -> True
  _otherRef      -> False
isSharedArticleRef _otherRef = False

isAllowed :: Permission -> Bool
isAllowed NotAllowed = False
isAllowed Allowed    = True

maybePerm :: Maybe Permission -> Permission
maybePerm (Just perm) = perm
maybePerm Nothing     = NotAllowed
