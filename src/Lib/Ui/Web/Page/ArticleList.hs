module Lib.Ui.Web.Page.ArticleList
  ( handler
  , query
  , view
  ) where

import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq

import           Lucid
import           Lucid.Servant                  ( linkAbsHref_ )
import           Servant                        ( Link
                                                , fieldLink
                                                )

import qualified Lib.Domain.Authorization      as Authz
import qualified Lib.Ui.Web.Page.Layout        as Layout
import qualified Lib.Ui.Web.Page.Static        as Static
import qualified Lib.Ui.Web.Page.ViewModel.Article
                                               as ArticleVm
import qualified Lib.Ui.Web.Route              as Route

import           Lib.Domain.Article             ( Article )
import           Lib.Domain.Authorization       ( ViewArticlesPerm )
import           Lib.Domain.Capability          ( ObjectReference )
import           Lib.Domain.Collection          ( Collection )
import           Lib.Domain.Id                  ( Id )
import           Lib.Infra.Error                ( throwOnError )
import           Lib.Ui.Web.Dto.Accesstoken     ( Accesstoken
                                                , Reference(..)
                                                )
import           Lib.Ui.Web.Page.Shared         ( WithQuery
                                                , createArticleForm
                                                , deleteArticleForm
                                                , getArticleMap
                                                , lookupReferences
                                                , markArticleAsReadForm
                                                , markArticleAsUnreadForm
                                                , navBar
                                                , prettyDate
                                                )
import           Lib.Ui.Web.Page.ViewModel.Article
                                                ( ArticleVm(..) )
import           Lib.Ui.Web.Route               ( HtmlPage )

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
  let canCreateArticles     = isRight $ Authz.canCreateArticles objRef
      canChangeArticleTitle = isRight $ Authz.canChangeTitles objRef
      canChangeArticleState = isRight $ Authz.canChangeStates objRef
      canDeleteArticle      = isRight $ Authz.canDeleteArticles objRef
      canShareLinks         = isRight $ Authz.canShareArticleList objRef
  viewArticlesPerm <- throwOnError $ Authz.canViewArticles objRef
  articles         <- getArticles colId
  pure View { .. }

getArticles :: WithQuery env m => Id Collection -> m (Seq ArticleVm)
getArticles = pure . fromMap . toArticleVm <=< getArticleMap
 where
  toArticleVm = fmap ArticleVm.fromDomain
  fromMap     = Seq.fromList . Map.elems

------------------------------------------------------------------------
-- View
------------------------------------------------------------------------

data View = View
  { viewArticlesPerm      :: !ViewArticlesPerm
  , acc                   :: !Accesstoken
  , canCreateArticles     :: !Bool
  , canChangeArticleTitle :: !Bool
  , canChangeArticleState :: !Bool
  , canDeleteArticle      :: !Bool
  , canShareLinks         :: !Bool
  , articles              :: !(Seq ArticleVm)
  }

view :: View -> Html ()
view View {..} = do
  navBar
    [ when canShareLinks . p_ $ do
        a_ [linkAbsHref_ . fieldLink Route.shareArticleListPage $ Just acc]
           "Share this page"
    ]

  when canCreateArticles newArticleH2

  h1_ "Your articles"
  div_ . ul_ [] . void $ traverse_
    (articleItem canChangeArticleTitle
                 canChangeArticleState
                 canDeleteArticle
                 acc
    )
    articles
 where
  newArticleH2 :: Html ()
  newArticleH2 = do
    h2_ "Add a new article"
    createArticleForm acc $ fieldLink Route.articleListPage (Just acc)

articleItem :: Bool -> Bool -> Bool -> Accesstoken -> ArticleVm -> Html ()
articleItem canChangeTitle canChangeState canDelete acc artVm =
  article_ [class_ "item"] $ do
    itemHeader
    itemMeta
 where
  itemHeader :: Html ()
  itemHeader = div_ [class_ "item-header"] $ do
    span_ [class_ "item-title"] $ do
      a_ [linkAbsHref_ . fieldLink Route.articlePage artId $ Just acc]
        . toHtml
        $ ArticleVm.title artVm
      when canChangeTitle $ small_ editArticleA

  itemMeta :: Html ()
  itemMeta = div_ [class_ "item-meta"] $ do
    itemMetaInfo
    itemMetaIcons

  itemMetaInfo :: Html ()
  itemMetaInfo = do
    let aCreationDate = prettyDate $ ArticleVm.creation artVm
    ul_ [class_ "item-meta-info"] $ do
      li_ . time_ [datetime_ aCreationDate] . toHtml $ aCreationDate
      li_ "|"
      li_
        . a_ [href_ $ ArticleVm.uri artVm]
        . small_
        . renderHostUrl
        $ ArticleVm.uriHost artVm

  itemMetaIcons :: Html ()
  itemMetaIcons = ul_ [class_ "item-meta-icons"] $ do
    when canDelete . li_ $ deleteArticleForm artId acc listArticlesLink

    case ArticleVm.state artVm of
      ArticleVm.Read -> when canChangeState $ do
        li_ "|"
        li_ $ markArticleAsUnreadForm artId acc listArticlesLink
      ArticleVm.Unread -> when canChangeState $ do
        li_ "|"
        li_ $ markArticleAsReadForm artId acc listArticlesLink

  listArticlesLink :: Link
  listArticlesLink = fieldLink Route.articleListPage $ Just acc

  renderHostUrl :: Maybe Text -> Html ()
  renderHostUrl = maybe "" toHtml

  artId :: Id Article
  artId = ArticleVm.id artVm

  editArticleA :: Html ()
  editArticleA =
    a_ [linkAbsHref_ . fieldLink Route.editArticlePage artId $ Just acc] "✏️"
