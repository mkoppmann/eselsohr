module Lib.Web.Route.SharedReference
  ( SharedRefs
  , SharedRefsSite(..)
  ) where

import           Lib.Core.Domain                ( Capability
                                                , Id
                                                )
import           Lib.Web.Types                  ( DeleteItemForm
                                                , Redirection
                                                , ToApi
                                                )
import           Servant                        ( (:>)
                                                , Capture
                                                , Delete
                                                , FormUrlEncoded
                                                , ReqBody
                                                )
import           Servant.API.Generic            ( GenericMode((:-)) )
import           Servant.HTML.Lucid             ( HTML )

newtype SharedRefsSite route = SharedRefsSite
  { deleteSharedRef ::
      route
        :- "api"
        :> "shared-refs"
        :> Capture "sharedRefId" (Id Capability)
        :> ReqBody '[FormUrlEncoded] DeleteItemForm
        :> Delete '[HTML] Redirection
  }
  deriving stock (Generic)

type SharedRefs = ToApi SharedRefsSite
