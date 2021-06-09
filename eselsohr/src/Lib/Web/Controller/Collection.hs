module Lib.Web.Controller.Collection
  ( collection
  ) where

import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( OverviewPerms(..)
                                                , render
                                                )
import qualified Lib.Core.Domain.Capability    as Cap
import           Lib.Core.Effect                ( MonadRandom(..)
                                                , MonadTime
                                                , RWState
                                                )
import qualified Lib.Core.Service              as Service
import           Lib.Web.Controller.Util        ( authAction
                                                , getContextState
                                                , getObjRef
                                                , notAuthorized
                                                , redirect
                                                )
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( AppServer
                                                , PostCreateSharedOverviewRefForm(..)
                                                , Redirection
                                                )
import           Servant                        ( fieldLink )

collection :: Route.CollectionsSite AppServer
collection = Route.CollectionsSite
  { Route.createCollection        = createCollection
  , Route.createSharedOverviewRef = createSharedOverviewRef
  }

createCollection :: (RWState m, MonadRandom m, WithError m) => m Redirection
createCollection = do
  acc <- Service.createCollection
  redirect . Route.linkAsText . fieldLink Route.collectionOverview $ Just acc

createSharedOverviewRef
  :: (RWState m, MonadRandom m, MonadTime m, WithError m)
  => PostCreateSharedOverviewRefForm
  -> m Redirection
createSharedOverviewRef PostCreateSharedOverviewRefForm {..} = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState

  sharedRefId <- getRandomId
  let mAuthAct = Cap.createSharedRef objRef sharedRefId
  sharedRef <- maybe notAuthorized pure
    $ Cap.createSharedOverviewRef objRef perms

  authAction
    (Service.createSharedRef ctxState petname expirationDate sharedRef)
    mAuthAct
  redirect $ render goto
 where
  perms :: OverviewPerms
  perms = OverviewPerms (Cap.maybePerm viewUnlockLinks)
                        (Cap.maybePerm createUnlockLinks)
                        (Cap.maybePerm delete)
                        (Cap.maybePerm Nothing)
