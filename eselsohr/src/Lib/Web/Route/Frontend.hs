module Lib.Web.Route.Frontend
  ( Frontend
  , FrontendSite(..)
  ) where

import           Clay                           ( Css )
import           Lib.Core.Domain                ( Accesstoken )
import           Lib.Web.Types                  ( DeleteActionForm
                                                , HtmlPage
                                                , PatchActionForm
                                                , PostActionForm
                                                , Redirection
                                                , ToApi
                                                )
import           Servant                        ( (:>)
                                                , Delete
                                                , FormUrlEncoded
                                                , Get
                                                , Patch
                                                , Post
                                                , QueryParam
                                                , ReqBody
                                                )
import           Servant.API.Generic            ( GenericMode((:-)) )
import           Servant.CSS.Clay               ( CSS )
import           Servant.HTML.Lucid             ( HTML )

data FrontendSite route = FrontendSite
  { startpage ::
      route
        :- Get '[HTML] HtmlPage
  , collectionMain ::
      route
        :- "collection"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , listArticles ::
      route
        :- "articles"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , showArticle ::
      route
        :- "articles"
        :> "read"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , editArticle ::
      route
        :- "articles"
        :> "edit"
        :> QueryParam "acc" Accesstoken
        :> Get '[HTML] HtmlPage
  , invalidToken ::
      route
        :- "invalid-token"
        :> Get '[HTML] HtmlPage
  , stylesheet ::
      route
        :- "static"
        :> "style.css"
        :> Get '[CSS] Css
  , createResource ::
      route
        :- "api"
        :> "new-resource"
        :> Post '[HTML] Redirection
  , deleteFrontend ::
      route
        :- "api"
        :> "cap"
        :> ReqBody '[FormUrlEncoded] DeleteActionForm
        :> Delete '[HTML] Redirection
  , patchFrontend ::
      route
        :- "api"
        :> "cap"
        :> ReqBody '[FormUrlEncoded] PatchActionForm
        :> Patch '[HTML] Redirection
  , postFrontend ::
      route
        :- "api"
        :> "cap"
        :> ReqBody '[FormUrlEncoded] PostActionForm
        :> Post '[HTML] Redirection
  }
  deriving stock (Generic)

type Frontend = ToApi FrontendSite
