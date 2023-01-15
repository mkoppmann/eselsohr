module Lib.Ui.Web.Page.ShareArticleList
    ( handler
    , query
    , view
    ) where

import Data.Time.Clock (UTCTime)
import Lucid
import Lucid.Servant (linkAbsHref_)
import Servant (fieldLink)

import Lib.Domain.Authorization qualified as Authz
import Lib.Domain.Capability qualified as Cap
import Lib.Ui.Web.Page.Layout qualified as Layout
import Lib.Ui.Web.Page.Static qualified as Static
import Lib.Ui.Web.Page.ViewModel.Capability qualified as CapVm
import Lib.Ui.Web.Page.ViewModel.UnlockLink qualified as UnlockLink
import Lib.Ui.Web.Route qualified as Route

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
    viewModel <- query $ Query objRef acc ref.collectionId
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
            articlesLinkA unlockLink.acc $ petname unlockLink.capVm
            deleteSharedArticleListRefForm unlockLink.capVm.id acc . fieldLink Route.shareArticleListPage $ Just acc

    articlesLinkA :: Accesstoken -> Text -> Html ()
    articlesLinkA artListAcc = a_ [linkAbsHref_ . fieldLink Route.articleListPage $ Just artListAcc] . toHtml

    getArticlesA :: Html ()
    getArticlesA = a_ [linkAbsHref_ . fieldLink Route.articleListPage $ Just acc] "Back to overview"

    petname :: CapabilityVm -> Text
    petname cap = fromMaybe (toText cap.id) cap.petname
