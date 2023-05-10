module Lib.Ui.Web.Page.CreateArticle
    ( handler
    , view
    ) where

import Lucid
import Servant (fieldLink)

import qualified Lib.Domain.Authorization as Authz
import qualified Lib.Ui.Web.Page.Layout as Layout
import qualified Lib.Ui.Web.Page.Static as Static
import qualified Lib.Ui.Web.Route as Route

import Lib.Domain.Authorization (CreateArticlesPerm)
import Lib.Infra.Error (throwOnError)
import Lib.Ui.Dto.Accesstoken (Accesstoken)
import Lib.Ui.Web.Page.Shared
    ( WithQuery
    , createArticleForm
    , lookupReferences
    )
import Lib.Ui.Web.Route (HtmlPage)

------------------------------------------------------------------------
-- Handler
------------------------------------------------------------------------

handler :: (WithQuery env m) => Maybe Accesstoken -> m HtmlPage
handler Nothing = Layout.renderM Static.notAuthorized
handler (Just acc) = do
    (_ref, objRef) <- lookupReferences acc
    createArticlesPerm <- throwOnError $ Authz.canCreateArticles objRef
    Layout.renderM $ view View{..}

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
    { createArticlesPerm :: !CreateArticlesPerm
    , acc :: !Accesstoken
    }

view :: View -> Html ()
view View{..} = do
    h1_ "Add a new article"
    createArticleForm acc goto
  where
    goto = fieldLink Route.createArticlePage $ Just acc
