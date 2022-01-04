module Lib.Ui.Web.Page.ShareCollectionOverview
  ( handler
  , query
  , view
  ) where


import           Data.Time.Clock                                      ( UTCTime )
import           Lucid
import           Lucid.Servant                                        ( linkAbsHref_ )
import           Servant                                              ( Link
                                                                      , fieldLink
                                                                      )

import qualified Lib.Domain.Authorization                            as Authz
import qualified Lib.Domain.Capability                               as Cap
import qualified Lib.Ui.Web.Page.Layout                              as Layout
import qualified Lib.Ui.Web.Page.Static                              as Static
import qualified Lib.Ui.Web.Page.ViewModel.Capability                as CapVm
import qualified Lib.Ui.Web.Page.ViewModel.UnlockLink                as UnlockLink
import qualified Lib.Ui.Web.Route                                    as Route

import           Lib.Domain.Authorization                             ( ShareUnlockLinksPerm )
import           Lib.Domain.Capability                                ( ObjectReference )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Infra.Error                                      ( throwOnError )
import           Lib.Ui.Web.Dto.Accesstoken                           ( Accesstoken
                                                                      , Reference(..)
                                                                      )
import           Lib.Ui.Web.Page.Shared                               ( WithQuery
                                                                      , createSharedOverviewRefForm
                                                                      , deleteSharedOverviewRefForm
                                                                      , getExpirationDates
                                                                      , getSharedLinks
                                                                      , lookupReferences
                                                                      , navBar
                                                                      )
import           Lib.Ui.Web.Page.ViewModel.Capability                 ( CapabilityVm )
import           Lib.Ui.Web.Page.ViewModel.Permission                 ( OverviewPermsVm(..) )
import           Lib.Ui.Web.Page.ViewModel.UnlockLink                 ( UnlockLinkVm )
import           Lib.Ui.Web.Route                                     ( HtmlPage )

------------------------------------------------------------------------
-- Handler
------------------------------------------------------------------------

handler :: WithQuery env m => Maybe Accesstoken -> m HtmlPage
handler Nothing    = Layout.renderM Static.notAuthorized
handler (Just acc) = do
  (ref, objRef) <- lookupReferences acc
  viewModel     <- query . Query objRef acc $ collectionId ref
  Layout.renderM $ view viewModel

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

data Query = Query
  { objRef :: !ObjectReference
  , acc    :: !Accesstoken
  , colId  :: !(Id Collection)
  }

query :: WithQuery env m => Query -> m View
query Query {..} = do
  let viewUnlockLinksPermVm   = isRight $ Authz.canViewUnlockLinks objRef
      createUnlockLinksPermVm = isRight $ Authz.canCreateUnlockLinks objRef
      deleteUnlockLinksPermVm = isRight $ Authz.canDeleteUnlockLinks objRef
      shareUnlockLinksPermVm  = isRight $ Authz.canShareUnlockLinks objRef
      sharingPerms            = OverviewPermsVm { .. }
  shareUnlockLinksPerm              <- throwOnError $ Authz.canShareUnlockLinks objRef
  (earliestExpDate, defaultExpDate) <- getExpirationDates
  sharedLinks                       <- getSharedLinks colId Cap.isSharedOverviewRef
  pure View { .. }

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
  { shareUnlockLinksPerm :: !ShareUnlockLinksPerm
  , acc                  :: !Accesstoken
  , sharingPerms         :: !OverviewPermsVm
  , earliestExpDate      :: !UTCTime
  , defaultExpDate       :: !UTCTime
  , sharedLinks          :: !(Seq UnlockLinkVm)
  }

view :: View -> Html ()
view View {..} = do
  navBar [collectionOverviewA acc "Back to overview"]

  h1_ "Collection Overview Sharing Menu"
  createSharedOverviewRefForm sharingPerms earliestExpDate defaultExpDate acc . fieldLink Route.shareOverviewPage $ Just
    acc
  div_ . void $ traverse_ sharedLinkItem sharedLinks
 where
  sharedLinkItem :: UnlockLinkVm -> Html ()
  sharedLinkItem unlockLink = do
    li_ $ do
      collectionOverviewA (unlockAcc unlockLink) . petname $ capVm unlockLink
      deleteSharedOverviewRefForm (CapVm.id $ capVm unlockLink) acc . fieldLink Route.shareOverviewPage $ Just acc

  petname :: CapabilityVm -> Text
  petname cap = fromMaybe (toText $ CapVm.id cap) $ CapVm.petname cap

  unlockAcc :: UnlockLinkVm -> Accesstoken
  unlockAcc = UnlockLink.acc

  capVm :: UnlockLinkVm -> CapabilityVm
  capVm = UnlockLink.capVm

  collectionOverviewA :: Accesstoken -> Text -> Html ()
  collectionOverviewA sharedAcc = a_ [linkAbsHref_ $ collectionUnlockLink sharedAcc] . toHtml

  collectionUnlockLink :: Accesstoken -> Link
  collectionUnlockLink = fieldLink Route.overviewPage . Just
