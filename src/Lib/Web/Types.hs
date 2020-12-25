-- | This module introduce aliases to use for @servant-generic@ types and functions writing.
module Lib.Web.Types
  ( AppServer,
    ToApi,
    HtmlPage,
    Redirection,
    DeleteActionForm (..),
    PatchActionForm (..),
    PostActionForm (..),
    ArticleWithTokens (..),
  )
where

import Lib.App (App)
import Lib.Core.Accesstoken (Accesstoken)
import Lib.Core.Article (Article)
import Lib.Core.Id (Id)
import Lib.Core.Uri (Uri)
import Lucid (Html)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)
import Web.FormUrlEncoded (FromForm)

type AppServer = AsServerT App

type ToApi (site :: Type -> Type) = ToServantApi site

type HtmlPage = Html ()

type Redirection = HtmlPage

newtype DeleteActionForm = DeleteActionForm
  { deleteafAccesstoken :: Id Accesstoken
  }
  deriving stock (Generic)

instance FromForm DeleteActionForm

data PatchActionForm = PatchActionForm
  { patchafAccesstoken :: !(Id Accesstoken),
    patchafArticleTitle :: !(Maybe LText)
  }
  deriving stock (Generic)

instance FromForm PatchActionForm

data PostActionForm = PostActionForm
  { postafAccesstoken :: !(Id Accesstoken),
    postafSecureLink :: !(Maybe Bool),
    postafNoExpiration :: !(Maybe Bool),
    postafUri :: !(Maybe Uri)
  }
  deriving stock (Generic)

instance FromForm PostActionForm

data ArticleWithTokens = ArticleWithTokens
  { awtArticle :: !Article,
    awtShow :: !(Id Accesstoken),
    awtEdit :: !(Id Accesstoken),
    awtChangeTitle :: !(Id Accesstoken),
    awtArchive :: !(Id Accesstoken),
    awtUnread :: !(Id Accesstoken),
    awtDelete :: !(Id Accesstoken)
  }
