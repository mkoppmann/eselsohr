module Lib.Web.Route.Action
  ( ActionApi,
    ActionSite (..),
  )
where

import Lib.Web.Types (DeleteActionForm, PatchActionForm, PostActionForm, Redirection, ToApi)
import Servant (Delete, FormUrlEncoded, Patch, Post, ReqBody, (:>))
import Servant.API.Generic (GenericMode ((:-)))
import Servant.HTML.Lucid (HTML)

data ActionSite route = ActionSite
  { deleteAction ::
      route
        :- "api"
        :> "cap"
        :> ReqBody '[FormUrlEncoded] DeleteActionForm
        :> Delete '[HTML] Redirection,
    patchAction ::
      route
        :- "api"
        :> "cap"
        :> ReqBody '[FormUrlEncoded] PatchActionForm
        :> Patch '[HTML] Redirection,
    postAction ::
      route
        :- "api"
        :> "cap"
        :> ReqBody '[FormUrlEncoded] PostActionForm
        :> Post '[HTML] Redirection
  }
  deriving stock (Generic)

type ActionApi = ToApi ActionSite
