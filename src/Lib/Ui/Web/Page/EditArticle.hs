module Lib.Ui.Web.Page.EditArticle
    ( handler
    , query
    , view
    ) where

import Lucid
import Servant (fieldLink)

import Lib.Domain.Authorization qualified as Authz
import Lib.Ui.Web.Page.Layout qualified as Layout
import Lib.Ui.Web.Page.Static qualified as Static
import Lib.Ui.Web.Page.ViewModel.Article qualified as ArticleVm
import Lib.Ui.Web.Route qualified as Route

import Lib.Domain.Article (Article)
import Lib.Domain.Authorization (ChangeTitlePerm)
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
    , changeArticleTitleForm
    , getArticle
    , lookupReferences
    )
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
    changeTitlePerm <- throwOnError $ Authz.canChangeArticleTitle objRef artId
    article <- getArticle colId artId
    let artTitle = article.title
    pure View{..}

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
    { changeTitlePerm :: !ChangeTitlePerm
    , acc :: !Accesstoken
    , artId :: !(Id Article)
    , artTitle :: !Text
    }

view :: View -> Html ()
view View{..} = do
    h1_ "Edit article title"
    changeArticleTitleForm artId artTitle acc goto
  where
    goto = fieldLink Route.articlePage artId $ Just acc
