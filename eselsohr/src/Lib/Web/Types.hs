{-# LANGUAGE DuplicateRecordFields #-}

module Lib.Web.Types
  ( AppServer
  , ToApi
  , HtmlPage
  , Redirection
  , CollectionOverviewData(..)
  , ArticleListData(..)
  , ShareOverviewLinkData(..)
  , ShareArticleListLinkData(..)
  , ShareArticleLinkData(..)
  , CreateSharedOverviewRefPerms(..)
  , CreateSharedArticlesRefPerms(..)
  , CreateSharedArticleRefPerms(..)
  , ViewArticleData(..)
  , DeleteItemForm(..)
  , PatchArticleForm(..)
  , PostCreateUnlockLinkForm(..)
  , PostCreateArticleForm(..)
  , PostCreateSharedOverviewRefForm(..)
  , PostCreateSharedArticlesRefForm(..)
  , PostCreateSharedArticleRefForm(..)
  ) where

import           Lib.App                        ( App )
import           Lib.Core.Domain                ( Accesstoken
                                                , Article
                                                , ArticleState
                                                , Capability
                                                , Entity
                                                , ExpirationDate
                                                , Permission
                                                , Revocable
                                                , Uri
                                                )
import           Lucid                          ( Html )
import           Servant.API.Generic            ( ToServantApi )
import           Servant.Server.Generic         ( AsServerT )
import           Web.FormUrlEncoded             ( FromForm )

type AppServer = AsServerT App

type ToApi (site :: Type -> Type) = ToServantApi site

type HtmlPage = Html ()

type Redirection = HtmlPage

data CollectionOverviewData = CollectionOverviewData
  { acc                 :: !Accesstoken
  , canCreateUnlockLink :: !Bool
  , canShareLinks       :: !Bool
  , earliestExpDate     :: !ExpirationDate
  , defaultExpDate      :: !ExpirationDate
  , unlockLinks         :: !(Seq (Capability, Revocable))
  }

data ArticleListData = ArticleListData
  { acc                   :: !Accesstoken
  , canCreateArticles     :: !Bool
  , canChangeArticleTitle :: !Bool
  , canChangeArticleState :: !Bool
  , canDeleteArticle      :: !Bool
  , canShareLinks         :: !Bool
  , articles              :: !(Seq (Entity Article))
  }

data ShareOverviewLinkData = ShareOverviewLinkData
  { acc             :: !Accesstoken
  , sharingPerms    :: !CreateSharedOverviewRefPerms
  , earliestExpDate :: !ExpirationDate
  , defaultExpDate  :: !ExpirationDate
  , sharedLinks     :: !(Seq (Capability, Revocable))
  }

data ShareArticleListLinkData = ShareArticleListLinkData
  { acc             :: !Accesstoken
  , canViewArticles :: !Bool
  , sharingPerms    :: !CreateSharedArticlesRefPerms
  , earliestExpDate :: !ExpirationDate
  , defaultExpDate  :: !ExpirationDate
  , sharedLinks     :: !(Seq (Capability, Revocable))
  }

data ShareArticleLinkData = ShareArticleLinkData
  { acc             :: !Accesstoken
  , canViewArticle  :: !Bool
  , sharingPerms    :: !CreateSharedArticleRefPerms
  , earliestExpDate :: !ExpirationDate
  , defaultExpDate  :: !ExpirationDate
  , sharedLinks     :: !(Seq (Capability, Revocable))
  }

data CreateSharedOverviewRefPerms = CreateSharedOverviewRefPerms
  { canViewUnlockLinks   :: !Bool
  , canCreateUnlockLinks :: !Bool
  , canDeleteUnlockLinks :: !Bool
  }

data CreateSharedArticlesRefPerms = CreateSharedArticlesRefPerms
  { canViewArticles       :: !Bool
  , canCreateArticles     :: !Bool
  , canChangeArticleTitle :: !Bool
  , canChangeArticleState :: !Bool
  , canDeleteArticle      :: !Bool
  }

data CreateSharedArticleRefPerms = CreateSharedArticleRefPerms
  { canViewArticle        :: !Bool
  , canChangeArticleTitle :: !Bool
  , canChangeArticleState :: !Bool
  , canDeleteArticle      :: !Bool
  }

data ViewArticleData = ViewArticleData
  { acc                   :: !Accesstoken
  , canViewArticles       :: !Bool
  , canChangeArticleTitle :: !Bool
  , canChangeArticleState :: !Bool
  , canDeleteArticle      :: !Bool
  , canShareLinks         :: !Bool
  , article               :: !(Entity Article)
  }

data DeleteItemForm = DeleteItemForm
  { acc  :: !Accesstoken
  , goto :: !Uri
  }
  deriving stock Generic
  deriving anyclass FromForm

data PatchArticleForm = PatchArticleForm
  { acc          :: !Accesstoken
  , goto         :: !Uri
  , articleTitle :: !(Maybe Text)
  , articleState :: !(Maybe ArticleState)
  }
  deriving stock Generic
  deriving anyclass FromForm

data PostCreateUnlockLinkForm = PostCreateUnlockLinkForm
  { acc            :: !Accesstoken
  , goto           :: !Uri
  , petname        :: !(Maybe Text)
  , expirationDate :: !(Maybe ExpirationDate)
  }
  deriving stock Generic
  deriving anyclass FromForm

data PostCreateArticleForm = PostCreateArticleForm
  { acc        :: !Accesstoken
  , goto       :: !Uri
  , articleUri :: !Uri
  }
  deriving stock Generic
  deriving anyclass FromForm

data PostCreateSharedOverviewRefForm = PostCreateSharedOverviewRefForm
  { acc               :: !Accesstoken
  , goto              :: !Uri
  , petname           :: !(Maybe Text)
  , expirationDate    :: !(Maybe ExpirationDate)
  , viewUnlockLinks   :: !(Maybe Permission)
  , createUnlockLinks :: !(Maybe Permission)
  , delete            :: !(Maybe Permission)
  }
  deriving stock Generic
  deriving anyclass FromForm

data PostCreateSharedArticlesRefForm = PostCreateSharedArticlesRefForm
  { acc            :: !Accesstoken
  , goto           :: !Uri
  , petname        :: !(Maybe Text)
  , expirationDate :: !(Maybe ExpirationDate)
  , viewArticles   :: !(Maybe Permission)
  , createArticles :: !(Maybe Permission)
  , changeTitle    :: !(Maybe Permission)
  , changeState    :: !(Maybe Permission)
  , delete         :: !(Maybe Permission)
  }
  deriving stock Generic
  deriving anyclass FromForm

data PostCreateSharedArticleRefForm = PostCreateSharedArticleRefForm
  { acc            :: !Accesstoken
  , goto           :: !Uri
  , petname        :: !(Maybe Text)
  , expirationDate :: !(Maybe ExpirationDate)
  , viewArticle    :: !(Maybe Permission)
  , changeTitle    :: !(Maybe Permission)
  , changeState    :: !(Maybe Permission)
  , delete         :: !(Maybe Permission)
  }
  deriving stock Generic
  deriving anyclass FromForm
