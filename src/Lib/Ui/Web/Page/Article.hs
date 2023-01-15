module Lib.Ui.Web.Page.Article
    ( handler
    , query
    , view
    ) where

import Lucid
import Lucid.Servant (linkAbsHref_)
import Servant
    ( Link
    , fieldLink
    )

import Lib.Domain.Authorization qualified as Authz
import Lib.Ui.Web.Page.Layout qualified as Layout
import Lib.Ui.Web.Page.Static qualified as Static
import Lib.Ui.Web.Page.ViewModel.Article qualified as ArticleVm
import Lib.Ui.Web.Route qualified as Route

import Lib.Domain.Article (Article)
import Lib.Domain.Authorization (ViewArticlePerm)
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
    , deleteArticleForm
    , getArticle
    , lookupReferences
    , markArticleAsReadForm
    , markArticleAsUnreadForm
    , navBar
    , prettyDate
    )
import Lib.Ui.Web.Page.ViewModel.Article (ArticleVm)
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
    let canViewArticles = isRight $ Authz.canViewArticles objRef
        canChangeArticleTitle = isRight $ Authz.canChangeArticleTitle objRef artId
        canChangeArticleState = isRight $ Authz.canChangeArticleState objRef artId
        canDeleteArticle = isRight $ Authz.canDeleteArticle objRef artId
        canShareLinks = isRight $ Authz.canShareArticle objRef artId
    viewArticlePerm <- throwOnError $ Authz.canViewArticle objRef artId
    article <- getArticle colId artId
    pure View{..}

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
    { viewArticlePerm :: !ViewArticlePerm
    , acc :: !Accesstoken
    , canViewArticles :: !Bool
    , canChangeArticleTitle :: !Bool
    , canChangeArticleState :: !Bool
    , canDeleteArticle :: !Bool
    , canShareLinks :: !Bool
    , article :: !ArticleVm
    }

view :: View -> Html ()
view View{..} = do
    navBar
        [ when canViewArticles getArticlesA
        , when canShareLinks $ do
            a_ [linkAbsHref_ . fieldLink Route.shareArticlePage article.id $ Just acc] "Share this page"
        ]

    h1_ . toHtml $ article.title
    when canChangeArticleTitle $ p_ editArticleA
    p_ . toHtml $ "Created: " <> prettyDate article.creation
    p_ . toHtml $ "State: " <> show @Text article.state
    p_ . a_ [href_ article.uri] $ toHtml article.uri
    when canDeleteArticle $
        if canViewArticles
            then deleteArticleForm article.id acc . fieldLink Route.articleListPage $ Just acc
            else deleteArticleForm article.id acc $ fieldLink Route.startpage
    case article.state of
        ArticleVm.Read -> when canChangeArticleState $ markArticleAsUnreadForm article.id acc showArticleLink
        ArticleVm.Unread -> when canChangeArticleState $ markArticleAsReadForm article.id acc showArticleLink
  where
    showArticleLink :: Link
    showArticleLink = fieldLink Route.articlePage article.id $ Just acc

    getArticlesA :: Html ()
    getArticlesA = a_ [linkAbsHref_ . fieldLink Route.articleListPage $ Just acc] "Back to overview"

    editArticleA :: Html ()
    editArticleA = a_ [linkAbsHref_ . fieldLink Route.editArticlePage article.id $ Just acc] "Edit title"
