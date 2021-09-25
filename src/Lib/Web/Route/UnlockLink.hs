module Lib.Web.Route.UnlockLink
  ( UnlockLinks
  , UnlockLinksSite(..)
  ) where

import           Lib.Core.Domain                ( Capability
                                                , Id
                                                )
import           Lib.Web.Types                  ( DeleteItemForm
                                                , PostCreateUnlockLinkForm
                                                , Redirection
                                                , ToApi
                                                )
import           Servant                        ( (:>)
                                                , Capture
                                                , Delete
                                                , FormUrlEncoded
                                                , Post
                                                , ReqBody
                                                )
import           Servant.API.Generic            ( GenericMode((:-)) )
import           Servant.HTML.Lucid             ( HTML )

data UnlockLinksSite route = UnlockLinksSite
  { createUnlockLink ::
      route
        :- "api"
        :> "unlock-links"
        :> ReqBody '[FormUrlEncoded] PostCreateUnlockLinkForm
        :> Post '[HTML] Redirection
  , deleteUnlockLink ::
      route
        :- "api"
        :> "unlock-links"
        :> Capture "unlockLinkId" (Id Capability)
        :> ReqBody '[FormUrlEncoded] DeleteItemForm
        :> Delete '[HTML] Redirection
  }
  deriving stock (Generic)

type UnlockLinks = ToApi UnlockLinksSite
