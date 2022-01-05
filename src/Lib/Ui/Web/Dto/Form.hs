module Lib.Ui.Web.Dto.Form
  ( CreateArticleForm(..)
  , ChangeArticleTitleForm(..)
  , ChangeArticleStateForm(..)
  , CreateUnlockLinkForm(..)
  , CreateSharedOverviewRefForm(..)
  , CreateSharedArticleListRefForm(..)
  , CreateSharedArticleRefForm(..)
  , DeleteItemForm(..)
  ) where

import           Web.FormUrlEncoded                                   ( FromForm
                                                                      , ToForm
                                                                      )

import           Lib.Ui.Web.Dto.Accesstoken                           ( Accesstoken )
import           Lib.Ui.Web.Dto.ExpirationDate                        ( ExpirationDate )

data CreateArticleForm = CreateArticleForm
  { acc        :: !Accesstoken
  , goto       :: !Text
  , articleUri :: !Text
  }
  deriving stock Generic
  deriving anyclass (FromForm, ToForm)

data ChangeArticleTitleForm = ChangeArticleTitleForm
  { acc          :: !Accesstoken
  , goto         :: !Text
  , articleTitle :: !Text
  }
  deriving stock Generic
  deriving anyclass (FromForm, ToForm)

data ChangeArticleStateForm = ChangeArticleStateForm
  { acc  :: !Accesstoken
  , goto :: !Text
  }
  deriving stock Generic
  deriving anyclass (FromForm, ToForm)

data CreateUnlockLinkForm = CreateUnlockLinkForm
  { acc            :: !Accesstoken
  , goto           :: !Text
  , petname        :: !(Maybe Text)
  , expirationDate :: !(Maybe ExpirationDate)
  }
  deriving stock Generic
  deriving anyclass (FromForm, ToForm)

data CreateSharedOverviewRefForm = CreateSharedOverviewRefForm
  { acc               :: !Accesstoken
  , goto              :: !Text
  , petname           :: !(Maybe Text)
  , expirationDate    :: !(Maybe ExpirationDate)
  , viewUnlockLinks   :: !(Maybe Bool)
  , createUnlockLinks :: !(Maybe Bool)
  , delete            :: !(Maybe Bool)
  }
  deriving stock Generic
  deriving anyclass (FromForm, ToForm)

data CreateSharedArticleListRefForm = CreateSharedArticleListRefForm
  { acc            :: !Accesstoken
  , goto           :: !Text
  , petname        :: !(Maybe Text)
  , expirationDate :: !(Maybe ExpirationDate)
  , viewArticles   :: !(Maybe Bool)
  , createArticles :: !(Maybe Bool)
  , changeTitle    :: !(Maybe Bool)
  , changeState    :: !(Maybe Bool)
  , delete         :: !(Maybe Bool)
  }
  deriving stock Generic
  deriving anyclass (FromForm, ToForm)

data CreateSharedArticleRefForm = CreateSharedArticleRefForm
  { acc            :: !Accesstoken
  , goto           :: !Text
  , petname        :: !(Maybe Text)
  , expirationDate :: !(Maybe ExpirationDate)
  , viewArticle    :: !(Maybe Bool)
  , changeTitle    :: !(Maybe Bool)
  , changeState    :: !(Maybe Bool)
  , delete         :: !(Maybe Bool)
  }
  deriving stock Generic
  deriving anyclass (FromForm, ToForm)

data DeleteItemForm = DeleteItemForm
  { acc  :: !Accesstoken
  , goto :: !Text
  }
  deriving stock Generic
  deriving anyclass (FromForm, ToForm)
