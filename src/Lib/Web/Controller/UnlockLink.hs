module Lib.Web.Controller.UnlockLink
  ( unlockLink
  ) where

import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( Capability
                                                , Id
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
                                                , redirect
                                                )
import qualified Lib.Web.Route.UnlockLink      as Route
import           Lib.Web.Types                  ( AppServer
                                                , DeleteItemForm(..)
                                                , PostCreateUnlockLinkForm(..)
                                                , Redirection
                                                )

unlockLink :: Route.UnlockLinksSite AppServer
unlockLink = Route.UnlockLinksSite { Route.createUnlockLink = createUnlockLink
                                   , Route.deleteUnlockLink = deleteUnlockLink
                                   }

createUnlockLink
  :: (RWState m, MonadRandom m, MonadTime m, WithError m)
  => PostCreateUnlockLinkForm
  -> m Redirection
createUnlockLink PostCreateUnlockLinkForm {..} = do
  ctxState <- getContextState acc
  uLinkId  <- getRandomId
  let mAuthAct = Cap.createUnlockLink (getObjRef ctxState) uLinkId
  authAction (Service.createUnlockLink ctxState petname expirationDate) mAuthAct
  redirect $ render goto

deleteUnlockLink
  :: (RWState m, MonadTime m, WithError m)
  => Id Capability
  -> DeleteItemForm
  -> m Redirection
deleteUnlockLink capId DeleteItemForm {..} = do
  ctxState <- getContextState acc
  let mAuthAct = Cap.deleteUnlockLink (getObjRef ctxState) capId
  authAction (Service.deleteUnlockLink ctxState) mAuthAct
  redirect $ render goto
