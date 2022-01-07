module Lib.Ui.Web.Page.EditArticle
  ( handler
  , query
  , view
  ) where


import           Lucid
import           Servant                                              ( fieldLink )

import qualified Lib.Domain.Authorization                            as Authz
import qualified Lib.Ui.Web.Page.Layout                              as Layout
import qualified Lib.Ui.Web.Page.Static                              as Static
import qualified Lib.Ui.Web.Page.ViewModel.Article                   as ArticleVm
import qualified Lib.Ui.Web.Route                                    as Route

import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.Authorization                             ( ChangeTitlePerm )
import           Lib.Domain.Capability                                ( ObjectReference )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Infra.Error                                      ( throwOnError )
import           Lib.Ui.Web.Dto.Accesstoken                           ( Accesstoken
                                                                      , Reference(..)
                                                                      )
import           Lib.Ui.Web.Page.Shared                               ( WithAppEnv
                                                                      , WithQuery
                                                                      , changeArticleTitleForm
                                                                      , getArticle
                                                                      , lookupReferences
                                                                      )
import           Lib.Ui.Web.Route                                     ( HtmlPage )

------------------------------------------------------------------------
-- Handler
------------------------------------------------------------------------

handler :: (WithAppEnv env m, WithQuery env m) => Id Article -> Maybe Accesstoken -> m HtmlPage
handler _artId Nothing    = Layout.renderM Static.notAuthorized
handler artId  (Just acc) = do
  (ref, objRef) <- lookupReferences acc
  viewModel     <- query . Query objRef acc artId $ collectionId ref
  Layout.renderM $ view viewModel

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

data Query = Query
  { objRef :: !ObjectReference
  , acc    :: !Accesstoken
  , artId  :: !(Id Article)
  , colId  :: !(Id Collection)
  }

query :: (WithAppEnv env m, WithQuery env m) => Query -> m View
query Query {..} = do
  changeTitlePerm <- throwOnError $ Authz.canChangeArticleTitle objRef artId
  article         <- getArticle colId artId
  let artTitle = ArticleVm.title article
  pure View { .. }

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
  { changeTitlePerm :: !ChangeTitlePerm
  , acc             :: !Accesstoken
  , artId           :: !(Id Article)
  , artTitle        :: !Text
  }

view :: View -> Html ()
view View {..} = do
  h1_ "Edit article title"
  changeArticleTitleForm artId artTitle acc goto
  where goto = fieldLink Route.articlePage artId $ Just acc
