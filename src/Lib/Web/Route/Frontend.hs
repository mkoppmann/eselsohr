module Lib.Web.Route.Frontend
  ( FrontendApi,
    FrontendSite (..),
  )
where

import Clay (Css)
import Lib.Core.Accesstoken (Accesstoken)
import Lib.Core.Id (Id)
import Lib.Web.Types (HtmlPage, ToApi)
import Servant (Get, QueryParam, (:>))
import Servant.API.Generic (GenericMode ((:-)))
import Servant.CSS.Clay (CSS)
import Servant.HTML.Lucid (HTML)

data FrontendSite route = FrontendSite
  { startpage ::
      route
        :- Get '[HTML] HtmlPage,
    listArticles ::
      route
        :- "articles"
        :> QueryParam "acc" (Id Accesstoken)
        :> Get '[HTML] HtmlPage,
    showArticle ::
      route
        :- "articles"
        :> "view"
        :> QueryParam "acc" (Id Accesstoken)
        :> Get '[HTML] HtmlPage,
    editArticle ::
      route
        :- "articles"
        :> "edit"
        :> QueryParam "acc" (Id Accesstoken)
        :> Get '[HTML] HtmlPage,
    stylesheet ::
      route
        :- "static"
        :> "style.css"
        :> Get '[CSS] Css
  }
  deriving stock (Generic)

type FrontendApi = ToApi FrontendSite
