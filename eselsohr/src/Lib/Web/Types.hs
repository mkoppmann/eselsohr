{-# LANGUAGE DuplicateRecordFields #-}

module Lib.Web.Types
  ( AppServer
  , ToApi
  , HtmlPage
  , Redirection
  , CollectionOverviewData(..)
  , ArticleListData(..)
  , ViewArticleData(..)
  , DeleteItemForm(..)
  , PatchArticleForm(..)
  , PostCreateUnlockLinkForm(..)
  , PostCreateArticleForm(..)
  ) where

import           Lib.App                        ( App )
import           Lib.Core.Domain                ( Accesstoken
                                                , Article
                                                , ArticleState
                                                , Capability
                                                , Entity
                                                , ExpirationDate
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
  , articles              :: !(Seq (Entity Article))
  }

data ViewArticleData = ViewArticleData
  { acc                   :: !Accesstoken
  , canViewArticles       :: !Bool
  , canChangeArticleTitle :: !Bool
  , canChangeArticleState :: !Bool
  , canDeleteArticle      :: !Bool
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
