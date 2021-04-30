module Lib.Web.Route.Command
  ( Command
  , CommandSite(..)
  ) where

import           Lib.Core.Domain                ( Accesstoken )
import           Lib.Web.Types                  ( ToApi )
import           Servant                        ( (:>)
                                                , DeleteNoContent
                                                , Header
                                                , PatchNoContent
                                                , PostNoContent
                                                )
import           Servant.API.Generic            ( GenericMode((:-)) )

data CommandSite route = CommandSite
  { createResourceApi ::
      route
        :- "api"
        :> "new-resource"
        :> PostNoContent
  , deleteApi ::
      route
        :- "api"
        :> "cap"
        :> Header "Authorization" Accesstoken
        :> DeleteNoContent
  , patchApi ::
      route
        :- "api"
        :> "cap"
        :> Header "Authorization" Accesstoken
        :> PatchNoContent
  , postApi ::
      route
        :- "api"
        :> "cap"
        :> Header "Authorization" Accesstoken
        :> PostNoContent
  }
  deriving stock (Generic)

type Command = ToApi CommandSite
