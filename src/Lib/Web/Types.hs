{-# LANGUAGE DuplicateRecordFields #-}

module Lib.Web.Types
  ( AppServer,
    ToApi,
    HtmlPage,
    Redirection,
    DeleteActionForm (..),
    PatchActionForm (..),
    PostActionForm (..),
  )
where

import Lib.App (App)
import Lib.Core.Domain (Accesstoken, ExpirationDate, Uri)
import Lucid (Html)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)
import Web.FormUrlEncoded (FromForm)

type AppServer = AsServerT App

type ToApi (site :: Type -> Type) = ToServantApi site

type HtmlPage = Html ()

type Redirection = HtmlPage

newtype DeleteActionForm = DeleteActionForm
  { acc :: Accesstoken
  }
  deriving stock (Generic)
  deriving anyclass (FromForm)

data PatchActionForm = PatchActionForm
  { acc :: !Accesstoken,
    articleTitle :: !(Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (FromForm)

data PostActionForm = PostActionForm
  { acc :: !Accesstoken,
    articleUri :: !(Maybe Uri),
    petname :: !(Maybe Text),
    expirationDate :: !(Maybe ExpirationDate)
  }
  deriving stock (Generic)
  deriving anyclass (FromForm)
