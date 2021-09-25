module Lib.Web.Route.Frontend
  ( Frontend
  , FrontendSite(..)
  ) where

import           Clay                           ( Css )
import           Lib.Core.Domain                ( Accesstoken
                                                , Article
                                                , Id
                                                )
import           Lib.Web.Types                  ( HtmlPage
                                                , ToApi
                                                )
import           Servant                        ( (:>)
                                                , Capture
                                                , Get
                                                , QueryParam
                                                )
import           Servant.API.Generic            ( GenericMode((:-)) )
import           Servant.CSS.Clay               ( CSS )
import           Servant.HTML.Lucid             ( HTML )

data FrontendSite route = FrontendSite
  { startpage ::
      route
        :- Get '[HTML] HtmlPage
  , invalidToken ::
      route
        :- "invalid-token"
        :> Get '[HTML] HtmlPage
  , collectionOverview ::
      route
        :- "resources"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , shareCollectionOverview ::
      route
        :- "resources"
        :> "shared-links"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , viewArticles ::
      route
        :- "articles"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , shareViewArticles ::
      route
        :- "articles"
        :> "shared-links"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , newArticle ::
      route
        :- "articles"
        :> "new"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , viewArticle ::
      route
        :- "articles"
        :> Capture "articleId" (Id Article)
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , shareViewArticle ::
      route
        :- "articles"
        :> Capture "articleId" (Id Article)
        :> "shared-links"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , editArticle ::
      route
        :- "articles"
        :> Capture "articleId" (Id Article)
        :> "edit"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , stylesheet ::
      route
        :- "static"
        :> "style.css"
        :> Get '[CSS] Css
  }
  deriving stock (Generic)

type Frontend = ToApi FrontendSite
