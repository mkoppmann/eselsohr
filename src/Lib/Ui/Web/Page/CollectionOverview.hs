module Lib.Ui.Web.Page.CollectionOverview
  ( handler
  , query
  , view
  ) where

import qualified Data.Map.Strict                                     as Map
import qualified Data.Sequence                                       as Seq

import           Data.Time.Clock                                      ( UTCTime )
import           Lucid
import           Lucid.Servant                                        ( linkAbsHref_ )
import           Servant                                              ( Link
                                                                      , fieldLink
                                                                      )

import qualified Lib.Domain.Authorization                            as Authz
import qualified Lib.Domain.Capability                               as Cap
import qualified Lib.Infra.Persistence.Model.Capability              as CapPm
import qualified Lib.Ui.Web.Page.Layout                              as Layout
import qualified Lib.Ui.Web.Page.Static                              as Static
import qualified Lib.Ui.Web.Page.ViewModel.Capability                as CapVm
import qualified Lib.Ui.Web.Page.ViewModel.UnlockLink                as UnlockLink
import qualified Lib.Ui.Web.Route                                    as Route

import           Lib.Domain.Authorization                             ( ViewUnlockLinksPerm )
import           Lib.Domain.Capability                                ( Capability
                                                                      , ObjectReference
                                                                      )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Infra.Error                                      ( throwOnError )
import           Lib.Ui.Web.Dto.Accesstoken                           ( Accesstoken
                                                                      , Reference(..)
                                                                      )
import           Lib.Ui.Web.Page.Shared                               ( WithQuery
                                                                      , capStillValid
                                                                      , createUnlockLinkForm
                                                                      , deleteUnlockLinkForm
                                                                      , getCapabilityMap
                                                                      , getExpirationDates
                                                                      , lookupReferences
                                                                      , navBar
                                                                      , prettyDate
                                                                      )
import           Lib.Ui.Web.Page.ViewModel.Capability                 ( CapabilityVm )
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
  let canCreateUnlockLink = isRight $ Authz.canCreateUnlockLinks objRef
      canDeleteUnlockLink = isRight $ Authz.canDeleteUnlockLinks objRef
      canShareLinks       = isRight $ Authz.canShareUnlockLinks objRef
  viewUnlockLinksPerm               <- throwOnError $ Authz.canViewUnlockLinks objRef
  (earliestExpDate, defaultExpDate) <- getExpirationDates
  unlockLinks                       <- getUnlockLinks colId earliestExpDate
  pure View { .. }

getUnlockLinks :: WithQuery env m => Id Collection -> UTCTime -> m (Seq UnlockLinkVm)
getUnlockLinks colId curTime = do
  capMap <- getCapabilityMap colId
  pure $ unlockLinkVms <$> sortedCaps capMap
 where
  sortedCaps :: Map a Capability -> Seq (a, Capability)
  sortedCaps = Seq.sortOn snd . Seq.filter (filterF . snd) . Seq.fromList . Map.toList

  filterF :: Capability -> Bool
  filterF cap = isStillValid cap && isViewArticles cap

  isStillValid :: Capability -> Bool
  isStillValid = isJust . capStillValid curTime . CapPm.fromDomain

  isViewArticles :: Capability -> Bool
  isViewArticles = isRight . Authz.canViewArticles . Cap.objectReference

  unlockLinkVms :: (Id Capability, Capability) -> UnlockLinkVm
  unlockLinkVms = uncurry (UnlockLink.fromDomain colId)

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
  { viewUnlockLinksPerm :: !ViewUnlockLinksPerm
  , acc                 :: !Accesstoken
  , canCreateUnlockLink :: !Bool
  , canDeleteUnlockLink :: !Bool
  , canShareLinks       :: !Bool
  , earliestExpDate     :: !UTCTime
  , defaultExpDate      :: !UTCTime
  , unlockLinks         :: !(Seq UnlockLinkVm)
  }

view :: View -> Html ()
view View {..} = do
  navBar
    [ when canShareLinks $ do
        a_ [linkAbsHref_ . fieldLink Route.shareOverviewPage $ Just acc] "Share this page"
    ]

  h1_ "Collection Overview Page"
  p_
    "Welcome to the main page of this collection.\
    \ You can unlock your article list here, as well as access the sharing\
    \ menu."
  p_
    "Bookmark this page for quick access or store it in your password manager.\
    \ If you loose access to this page, you can cannot access it again. Treat\
    \ the link like a password.\
    \ Do not share it with anyone."

  h2_ "Currently active unlock links"
  if null unlockLinks
    then p_ "You have no active access links for this collection"
    else ul_ $ traverse_ renderActiveLinks unlockLinks
  h2_ "Unlock article list"
  p_
    "Here you can generate a new token for accessing your article list.\
    \ They are valid for one month, unless you choose something else."
  if canCreateUnlockLink
    then createUnlockLinkForm earliestExpDate defaultExpDate acc collectionUnlockLink
    else p_ "You do not have the permission to create new access links."
 where
  renderActiveLinks :: UnlockLinkVm -> Html ()
  renderActiveLinks unlockLink = li_ $ do
    articlesLinkA (unlockAcc unlockLink) . petname $ capVm unlockLink
    p_ . small_ $ "Expires on: " <> expDate (capVm unlockLink)
    when canDeleteUnlockLink $ do
      deleteUnlockLinkForm (CapVm.id $ capVm unlockLink) acc collectionUnlockLink

  petname :: CapabilityVm -> Text
  petname cap = fromMaybe (toText $ CapVm.id cap) $ CapVm.petname cap

  expDate :: CapabilityVm -> Html ()
  expDate = maybe "Never" (toHtml . prettyDate) . CapVm.expirationDate

  capVm :: UnlockLinkVm -> CapabilityVm
  capVm = UnlockLink.capVm

  unlockAcc :: UnlockLinkVm -> Accesstoken
  unlockAcc = UnlockLink.acc

  collectionUnlockLink :: Link
  collectionUnlockLink = fieldLink Route.overviewPage $ Just acc

  articlesLinkA :: Accesstoken -> Text -> Html ()
  articlesLinkA artListAcc = a_ [linkAbsHref_ . fieldLink Route.articleListPage $ Just artListAcc] . toHtml
