module Lib.Ui.Web.Page.ShareArticle
    ( handler
    , query
    , view
    ) where

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

import Data.Time.Clock (UTCTime)
import Lib.Domain.Article (Article)
import Lib.Domain.Authorization (ShareArticlePerm)
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
    , createSharedArticleRefForm
    , deleteSharedArticleRefForm
    , getExpirationDates
    , getSharedLinks
    , lookupReferences
    , navBar
    )
import Lib.Ui.Web.Page.ViewModel.Capability (CapabilityVm)
import Lib.Ui.Web.Page.ViewModel.Permission (ArticlePermsVm (..))
import Lib.Ui.Web.Page.ViewModel.UnlockLink (UnlockLinkVm)
import Lib.Ui.Web.Route (HtmlPage)

------------------------------------------------------------------------
-- Handler
------------------------------------------------------------------------

handler :: WithQuery env m => Id Article -> Maybe Accesstoken -> m HtmlPage
handler _artId Nothing = Layout.renderM Static.notAuthorized
handler artId (Just acc) = do
    (ref, objRef) <- lookupReferences acc
    viewModel <- query $ Query objRef acc artId ref.collectionId
    Layout.renderM $ view viewModel

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

data Query = Query
    { objRef :: !ObjectReference
    , acc :: !Accesstoken
    , artId :: !(Id Article)
    , colId :: !(Id Collection)
    }

query :: WithQuery env m => Query -> m View
query Query{..} = do
    let canViewArticle = isRight $ Authz.canViewArticle objRef artId
        viewArticlePermVm = isRight $ Authz.canViewArticle objRef artId
        changeTitlePermVm = isRight $ Authz.canChangeArticleTitle objRef artId
        changeStatePermVm = isRight $ Authz.canChangeArticleState objRef artId
        deleteArticlePermVm = isRight $ Authz.canDeleteArticle objRef artId
        shareArticlePermVm = isRight $ Authz.canShareArticle objRef artId
        sharingPerms = ArticlePermsVm{..}
    shareArticlePerm <- throwOnError $ Authz.canShareArticle objRef artId
    (earliestExpDate, defaultExpDate) <- getExpirationDates
    sharedLinks <- getSharedLinks colId Cap.isSharedArticleRef
    pure View{..}

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
    { shareArticlePerm :: !ShareArticlePerm
    , acc :: !Accesstoken
    , canViewArticle :: !Bool
    , artId :: !(Id Article)
    , sharingPerms :: !ArticlePermsVm
    , earliestExpDate :: !UTCTime
    , defaultExpDate :: !UTCTime
    , sharedLinks :: !(Seq UnlockLinkVm)
    }

view :: View -> Html ()
view View{..} = do
    navBar [when canViewArticle $ articleLinkA acc "Back to article"]

    h1_ "Article Sharing Menu"
    createSharedArticleRefForm artId sharingPerms earliestExpDate defaultExpDate acc
        . fieldLink Route.shareArticlePage artId
        $ Just acc
    div_ . void $ traverse_ sharedLinkItem sharedLinks
  where
    sharedLinkItem :: UnlockLinkVm -> Html ()
    sharedLinkItem unlockLink = do
        li_ $ do
            articleLinkA unlockLink.acc $ petname unlockLink.capVm
            deleteSharedArticleRefForm artId unlockLink.capVm.id acc . fieldLink Route.shareArticlePage artId $
                Just
                    acc

    petname :: CapabilityVm -> Text
    petname cap = fromMaybe (toText cap.id) cap.petname

    articleLinkA :: Accesstoken -> Text -> Html ()
    articleLinkA artListAcc = a_ [linkAbsHref_ . fieldLink Route.articlePage artId $ Just artListAcc] . toHtml
