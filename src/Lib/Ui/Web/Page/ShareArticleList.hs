module Lib.Ui.Web.Page.ShareArticleList
    ( handler
    , query
    , view
    ) where

import Data.Time.Clock (UTCTime)
import Lucid
import Lucid.Servant (linkAbsHref_)
import Servant (fieldLink)

import qualified Lib.Domain.Authorization as Authz
import qualified Lib.Domain.Capability as Cap
import qualified Lib.Ui.Web.Page.Layout as Layout
import qualified Lib.Ui.Web.Page.Static as Static
import qualified Lib.Ui.Web.Page.ViewModel.Capability as CapVm
import qualified Lib.Ui.Web.Page.ViewModel.UnlockLink as UnlockLink
import qualified Lib.Ui.Web.Route as Route

import Lib.Domain.Authorization (ShareArticleListPerm)
import Lib.Domain.Capability (ObjectReference)
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)
import Lib.Infra.Error (throwOnError)
import Lib.Ui.Dto.Accesstoken
    ( Accesstoken
    , Reference (..)
    )
import Lib.Ui.Web.Page.Shared
    ( WithQuery
    , createSharedArticleListRefForm
    , deleteSharedArticleListRefForm
    , getExpirationDates
    , getSharedLinks
    , lookupReferences
    , navBar
    )
import Lib.Ui.Web.Page.ViewModel.Capability (CapabilityVm)
import Lib.Ui.Web.Page.ViewModel.Permission (ArticlesPermsVm (..))
import Lib.Ui.Web.Page.ViewModel.UnlockLink (UnlockLinkVm)
import Lib.Ui.Web.Route (HtmlPage)

------------------------------------------------------------------------
-- Handler
------------------------------------------------------------------------

handler :: WithQuery env m => Maybe Accesstoken -> m HtmlPage
handler Nothing = Layout.renderM Static.notAuthorized
handler (Just acc) = do
    (ref, objRef) <- lookupReferences acc
    viewModel <- query . Query objRef acc $ collectionId ref
    Layout.renderM $ view viewModel

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

data Query = Query
    { objRef :: !ObjectReference
    , acc :: !Accesstoken
    , colId :: !(Id Collection)
    }

query :: WithQuery env m => Query -> m View
query Query{..} = do
    let canViewArticles = isRight $ Authz.canViewArticles objRef
        viewArticlesPermVm = isRight $ Authz.canViewArticles objRef
        createArticlesPermVm = isRight $ Authz.canCreateArticles objRef
        changeTitlesPermVm = isRight $ Authz.canChangeTitles objRef
        changeStatesPermVm = isRight $ Authz.canChangeStates objRef
        deleteArticlesPermVm = isRight $ Authz.canDeleteArticles objRef
        shareArticleListPermVm = isRight $ Authz.canShareArticleList objRef
        sharingPerms = ArticlesPermsVm{..}
    shareArticleListPerm <- throwOnError $ Authz.canShareArticleList objRef
    (earliestExpDate, defaultExpDate) <- getExpirationDates
    sharedLinks <- getSharedLinks colId Cap.isSharedArticlesRef
    pure View{..}

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
    { shareArticleListPerm :: !ShareArticleListPerm
    , acc :: !Accesstoken
    , canViewArticles :: !Bool
    , sharingPerms :: !ArticlesPermsVm
    , earliestExpDate :: !UTCTime
    , defaultExpDate :: !UTCTime
    , sharedLinks :: !(Seq UnlockLinkVm)
    }

view :: View -> Html ()
view View{..} = do
    navBar [when canViewArticles getArticlesA]

    h1_ "Article List Sharing Menu"
    createSharedArticleListRefForm sharingPerms earliestExpDate defaultExpDate acc
        . fieldLink Route.shareArticleListPage
        $ Just acc
    div_ . void $ traverse_ sharedLinkItem sharedLinks
  where
    sharedLinkItem :: UnlockLinkVm -> Html ()
    sharedLinkItem unlockLink = do
        li_ $ do
            articlesLinkA (unlockAcc unlockLink) . petname $ capVm unlockLink
            deleteSharedArticleListRefForm (CapVm.id $ capVm unlockLink) acc . fieldLink Route.shareArticleListPage $ Just acc

    unlockAcc :: UnlockLinkVm -> Accesstoken
    unlockAcc = UnlockLink.acc

    capVm :: UnlockLinkVm -> CapabilityVm
    capVm = UnlockLink.capVm

    articlesLinkA :: Accesstoken -> Text -> Html ()
    articlesLinkA artListAcc = a_ [linkAbsHref_ . fieldLink Route.articleListPage $ Just artListAcc] . toHtml

    getArticlesA :: Html ()
    getArticlesA = a_ [linkAbsHref_ . fieldLink Route.articleListPage $ Just acc] "Back to overview"

    petname :: CapabilityVm -> Text
    petname cap = fromMaybe (toText $ CapVm.id cap) $ CapVm.petname cap
