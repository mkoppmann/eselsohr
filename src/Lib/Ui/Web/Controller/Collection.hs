module Lib.Ui.Web.Controller.Collection
  ( collection
  ) where

import           Servant.Links                                        ( fieldLink )

import qualified Lib.App.Command                                     as Command
import qualified Lib.Ui.Web.Page.CollectionOverview                  as CollectionOverviewPage
import qualified Lib.Ui.Web.Page.ShareCollectionOverview             as ShareCollectionOverviewPage
import qualified Lib.Ui.Web.Route                                    as Route

import           Lib.App.Port                                         ( MonadRandom )
import           Lib.Domain.Capability                                ( Capability
                                                                      , mkOverviewPerms
                                                                      )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Repo.CapabilityList                       ( CapabilityListRepo )
import           Lib.Domain.Repo.Collection                           ( CollectionRepo )
import           Lib.Infra.Error                                      ( WithError
                                                                      , redirectTo
                                                                      , throwOnErrorM
                                                                      )
import           Lib.Ui.Web.Dto.Accesstoken                           ( Reference(..)
                                                                      , mkAccesstoken
                                                                      )
import           Lib.Ui.Web.Dto.ExpirationDate                        ( ExpirationDate(..) )
import           Lib.Ui.Web.Dto.Form                                  ( CreateSharedOverviewRefForm(..)
                                                                      , CreateUnlockLinkForm(..)
                                                                      , DeleteItemForm(..)
                                                                      )
import           Lib.Ui.Web.Page.Shared                               ( WithQuery
                                                                      , lookupReferences
                                                                      )
import           Lib.Ui.Web.Route                                     ( AppServer
                                                                      , CollectionSite
                                                                      , Redirection
                                                                      )

collection :: CollectionSite AppServer
collection = Route.CollectionSite { Route.createCollection        = createCollection
                                  , Route.overviewPage            = CollectionOverviewPage.handler
                                  , Route.createUnlockLink        = createUnlockLink
                                  , Route.deleteUnlockLink        = deleteUnlockLink
                                  , Route.shareOverviewPage       = ShareCollectionOverviewPage.handler
                                  , Route.createSharedOverviewRef = createSharedOverviewRef
                                  , Route.deleteSharedOverviewRef = deleteSharedOverviewRef
                                  }

createCollection :: (CollectionRepo m, MonadRandom m, WithError m) => m Redirection
createCollection = do
  (colId, capId) <- Command.createCollection
  let acc = mkAccesstoken $ Reference colId capId
      url = Route.linkAsText . fieldLink Route.overviewPage $ Just acc
  redirectTo url

createUnlockLink :: (CapabilityListRepo m, WithQuery env m) => CreateUnlockLinkForm -> m Redirection
createUnlockLink CreateUnlockLinkForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.createUnlockLink command
  redirectTo goto
 where
  mkCommand objRef colId = do
    let mPetname = petname
        mExpDate = unExpirationDate <$> expirationDate
    Command.CreateUnlockLink { .. }

deleteUnlockLink :: (CapabilityListRepo m, WithQuery env m) => Id Capability -> DeleteItemForm -> m Redirection
deleteUnlockLink capId DeleteItemForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.deleteUnlockLink command
  redirectTo goto
  where mkCommand objRef colId = Command.DeleteUnlockLink { .. }

createSharedOverviewRef :: (CapabilityListRepo m, WithQuery env m) => CreateSharedOverviewRefForm -> m Redirection
createSharedOverviewRef CreateSharedOverviewRefForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.addShareUnlockLinks command
  redirectTo goto
 where
  mkCommand objRef colId = do
    let mPetname    = petname
        mExpDate    = unExpirationDate <$> expirationDate
        share       = False
        sharedPerms = mkOverviewPerms (isJust viewUnlockLinks) (isJust createUnlockLinks) (isJust delete) share
    Command.AddShareUnlockLinks { .. }

deleteSharedOverviewRef :: (CapabilityListRepo m, WithQuery env m) => Id Capability -> DeleteItemForm -> m Redirection
deleteSharedOverviewRef capId DeleteItemForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.deleteShareUnlockLinks command
  redirectTo goto
  where mkCommand objRef colId = Command.DeleteShareUnlockLinks { .. }
