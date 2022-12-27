module Lib.Domain.Capability
    ( -- * Capability
      Capability (..)
    , mkCapability

      -- * Object references
    , ObjectReference (..)
    , SharedReference (..)
    , OverviewPerms (..)
    , ArticlesPerms (..)
    , ArticlePerms (..)
    , mkOverviewPerms
    , mkArticlesPerms
    , mkArticlePerms
    , isSharedRef
    , isSharedOverviewRef
    , isSharedArticlesRef
    , isSharedArticleRef

      -- * Unlock link permissions
    , ViewUnlockLinks (..)
    , CreateUnlockLinks (..)
    , DeleteUnlockLinks (..)
    , ShareUnlockLinks (..)
    , defaultOverviewRef
    , createSharedOverviewRef

      -- * Article list permissions
    , ViewArticles (..)
    , CreateArticles (..)
    , ChangeTitles (..)
    , ChangeStates (..)
    , DeleteArticles (..)
    , ShareArticleList (..)
    , defaultArticlesRef
    , createSharedArticlesRef

      -- * Article permissions
    , ViewArticle (..)
    , ChangeTitle (..)
    , ChangeState (..)
    , DeleteArticle (..)
    , ShareArticle (..)
    , createSharedArticleRef
    ) where

import qualified Data.Text as Text

import Data.Time.Clock (UTCTime)
import Prelude hiding (id)

import Lib.Domain.Article (Article)
import Lib.Domain.Id (Id)

------------------------------------------------------------------------
-- Capability
------------------------------------------------------------------------

data Capability = Capability
    { id :: !(Id Capability)
    , objectReference :: !ObjectReference
    , petname :: !(Maybe Text)
    , expirationDate :: !(Maybe UTCTime)
    }
    deriving stock (Show)

instance Eq Capability where
    (==) (Capability aId _ _ _) (Capability bId _ _ _) = aId == bId

mkCapability :: Id Capability -> ObjectReference -> Maybe Text -> Maybe UTCTime -> Capability
mkCapability id objectReference mPetname expirationDate = Capability{..}
  where
    petname = case mPetname of
        Nothing -> Nothing
        (Just pName) -> if Text.null pName then Nothing else Just pName

------------------------------------------------------------------------
-- Object references
------------------------------------------------------------------------

data ObjectReference
    = OverviewRef !OverviewPerms
    | ArticlesRef !ArticlesPerms
    | ArticleRef !(Id Article) !ArticlePerms
    | SharedRef !SharedReference
    deriving stock (Eq, Read, Show)

newtype SharedReference = SharedReference
    { sharedObjRef :: ObjectReference
    }
    deriving stock (Eq, Read, Show)

data OverviewPerms = OverviewPerms
    { viewUnlockLinksPerm :: !(Maybe ViewUnlockLinks)
    , createUnlockLinksPerm :: !(Maybe CreateUnlockLinks)
    , deleteUnlockLinksPerm :: !(Maybe DeleteUnlockLinks)
    , shareUnlockLinksPerm :: !(Maybe ShareUnlockLinks)
    }
    deriving stock (Eq, Ord, Read, Show)

data ArticlesPerms = ArticlesPerms
    { viewArticlesPerm :: !(Maybe ViewArticles)
    , createArticlesPerm :: !(Maybe CreateArticles)
    , changeTitlesPerm :: !(Maybe ChangeTitles)
    , changeStatesPerm :: !(Maybe ChangeStates)
    , deleteArticlesPerm :: !(Maybe DeleteArticles)
    , shareArticleListPerm :: !(Maybe ShareArticleList)
    }
    deriving stock (Eq, Ord, Read, Show)

data ArticlePerms = ArticlePerms
    { viewArticlePerm :: !(Maybe ViewArticle)
    , changeTitlePerm :: !(Maybe ChangeTitle)
    , changeStatePerm :: !(Maybe ChangeState)
    , deleteArticlePerm :: !(Maybe DeleteArticle)
    , shareArticlePerm :: !(Maybe ShareArticle)
    }
    deriving stock (Eq, Ord, Read, Show)

mkOverviewPerms :: Bool -> Bool -> Bool -> Bool -> OverviewPerms
mkOverviewPerms canView canCreate canDelete canShare = OverviewPerms{..}
  where
    viewUnlockLinksPerm = if canView then Just ViewUnlockLinks else Nothing
    createUnlockLinksPerm = if canCreate then Just CreateUnlockLinks else Nothing
    deleteUnlockLinksPerm = if canDelete then Just DeleteUnlockLinks else Nothing
    shareUnlockLinksPerm = if canShare then Just ShareUnlockLinks else Nothing

mkArticlesPerms :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> ArticlesPerms
mkArticlesPerms canView canCreate canChangeTitles canChangeStates canDelete canShare = ArticlesPerms{..}
  where
    viewArticlesPerm = if canView then Just ViewArticles else Nothing
    createArticlesPerm = if canCreate then Just CreateArticles else Nothing
    changeTitlesPerm = if canChangeTitles then Just ChangeTitles else Nothing
    changeStatesPerm = if canChangeStates then Just ChangeStates else Nothing
    deleteArticlesPerm = if canDelete then Just DeleteArticles else Nothing
    shareArticleListPerm = if canShare then Just ShareArticleList else Nothing

mkArticlePerms :: Id Article -> Bool -> Bool -> Bool -> Bool -> Bool -> ArticlePerms
mkArticlePerms artId canView canChangeTitle canChangeState canDelete canShare = ArticlePerms{..}
  where
    viewArticlePerm = if canView then Just $ ViewArticle artId else Nothing
    changeTitlePerm = if canChangeTitle then Just $ ChangeTitle artId else Nothing
    changeStatePerm = if canChangeState then Just $ ChangeState artId else Nothing
    deleteArticlePerm = if canDelete then Just $ DeleteArticle artId else Nothing
    shareArticlePerm = if canShare then Just $ ShareArticle artId else Nothing

isSharedRef :: ObjectReference -> Bool
isSharedRef (SharedRef _) = True
isSharedRef _otherRef = False

isSharedOverviewRef :: ObjectReference -> Bool
isSharedOverviewRef (SharedRef SharedReference{..}) = case sharedObjRef of
    OverviewRef _ -> True
    _otherRef -> False
isSharedOverviewRef _otherRef = False

isSharedArticlesRef :: ObjectReference -> Bool
isSharedArticlesRef (SharedRef SharedReference{..}) = case sharedObjRef of
    ArticlesRef _ -> True
    _otherRef -> False
isSharedArticlesRef _otherRef = False

isSharedArticleRef :: ObjectReference -> Bool
isSharedArticleRef (SharedRef SharedReference{..}) = case sharedObjRef of
    ArticleRef _ _ -> True
    _otherRef -> False
isSharedArticleRef _otherRef = False

------------------------------------------------------------------------
-- Unlock link permissions
------------------------------------------------------------------------

data ViewUnlockLinks = ViewUnlockLinks
    deriving stock (Eq, Ord, Read, Show)
data CreateUnlockLinks = CreateUnlockLinks
    deriving stock (Eq, Ord, Read, Show)
data DeleteUnlockLinks = DeleteUnlockLinks
    deriving stock (Eq, Ord, Read, Show)
data ShareUnlockLinks = ShareUnlockLinks
    deriving stock (Eq, Ord, Read, Show)

defaultOverviewRef :: ObjectReference
defaultOverviewRef = OverviewRef defaultOverviewPerms

defaultOverviewPerms :: OverviewPerms
defaultOverviewPerms =
    OverviewPerms (Just ViewUnlockLinks) (Just CreateUnlockLinks) (Just DeleteUnlockLinks) (Just ShareUnlockLinks)

createSharedOverviewRef :: ObjectReference -> OverviewPerms -> Maybe ObjectReference
createSharedOverviewRef (OverviewRef ogPerms) newPerms =
    if ogPerms >= newPerms then pure . SharedRef . SharedReference $ OverviewRef newPerms else Nothing
createSharedOverviewRef _otherRef _newPerms = Nothing

------------------------------------------------------------------------
-- Article list permissions
------------------------------------------------------------------------

data ViewArticles = ViewArticles
    deriving stock (Eq, Ord, Read, Show)
data CreateArticles = CreateArticles
    deriving stock (Eq, Ord, Read, Show)
data ChangeTitles = ChangeTitles
    deriving stock (Eq, Ord, Read, Show)
data ChangeStates = ChangeStates
    deriving stock (Eq, Ord, Read, Show)
data DeleteArticles = DeleteArticles
    deriving stock (Eq, Ord, Read, Show)
data ShareArticleList = ShareArticleList
    deriving stock (Eq, Ord, Read, Show)

defaultArticlesRef :: ObjectReference
defaultArticlesRef = ArticlesRef defaultArticlesPermissions

defaultArticlesPermissions :: ArticlesPerms
defaultArticlesPermissions =
    ArticlesPerms
        (Just ViewArticles)
        (Just CreateArticles)
        (Just ChangeTitles)
        (Just ChangeStates)
        (Just DeleteArticles)
        (Just ShareArticleList)

createSharedArticlesRef :: ObjectReference -> ArticlesPerms -> Maybe ObjectReference
createSharedArticlesRef (ArticlesRef ogPerms) newPerms =
    if ogPerms >= newPerms then pure . SharedRef . SharedReference $ ArticlesRef newPerms else Nothing
createSharedArticlesRef _otherRef _newPerms = Nothing

------------------------------------------------------------------------
-- Article permissions
------------------------------------------------------------------------

newtype ViewArticle = ViewArticle (Id Article)
    deriving stock (Eq, Ord, Read, Show)
newtype ChangeTitle = ChangeTitle (Id Article)
    deriving stock (Eq, Ord, Read, Show)
newtype ChangeState = ChangeState (Id Article)
    deriving stock (Eq, Ord, Read, Show)
newtype DeleteArticle = DeleteArticle (Id Article)
    deriving stock (Eq, Ord, Read, Show)
newtype ShareArticle = ShareArticle (Id Article)
    deriving stock (Eq, Ord, Read, Show)

createSharedArticleRef :: ObjectReference -> ArticlePerms -> Id Article -> Maybe ObjectReference
createSharedArticleRef (ArticlesRef ogPerms) newPerms articleId =
    let ArticlesPerms{..} = ogPerms
        ArticlePerms{..} = newPerms
        permToBool = bimap isJust isJust
        permissionList =
            [ permToBool (viewArticlesPerm, viewArticlePerm)
            , permToBool (changeTitlesPerm, changeTitlePerm)
            , permToBool (changeStatesPerm, changeStatePerm)
            , permToBool (deleteArticlesPerm, deleteArticlePerm)
            , permToBool (shareArticleListPerm, shareArticlePerm)
            ]
        validPermissions = all (uncurry (>=)) permissionList
     in if validPermissions then pure . SharedRef . SharedReference $ ArticleRef articleId newPerms else Nothing
createSharedArticleRef (ArticleRef artId ogPerms) newPerms compareId =
    if ogPerms >= newPerms && artId == compareId
        then pure . SharedRef . SharedReference $ ArticleRef artId newPerms
        else Nothing
createSharedArticleRef _otherRef _newPerms _compareId = Nothing
