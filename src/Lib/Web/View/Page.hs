module Lib.Web.View.Page
  ( root,
    articles,
    showArticle,
    editArticle,
  )
where

import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Lens.Micro ((^?), _Right)
import Lib.Core.Accesstoken (Accesstoken)
import Lib.Core.Article (Article (..), ArticleState (..))
import Lib.Core.Id (Id)
import Lib.Core.Uri (Uri (unUri))
import Lib.Web.Types (ArticleWithTokens (..))
import qualified Lib.Web.View.Form as Form
import Lucid
import Servant (Link)
import qualified Text.URI as URI
import qualified Text.URI.Lens as UL

root :: Link -> Id Accesstoken -> Html ()
root actionR newCollAcc = do
  h1_ "Welcome to Eselsohr"
  p_
    "Eselsohr is a service focussed on simplicity.\
    \ Save web articles and consume them later.\
    \ Start your collection by clicking on the button."
  Form.newCollection actionR newCollAcc

articles :: Link -> Link -> Link -> Id Accesstoken -> [ArticleWithTokens] -> Html ()
articles showArticleR editArticleR actionR insertAcc articleWithTokens = do
  newArticle actionR insertAcc
  articleList showArticleR editArticleR actionR articleWithTokens

showArticle :: Link -> Link -> ArticleWithTokens -> Html ()
showArticle editArticleR actionR ArticleWithTokens {..} = do
  let aUrl = articleUrl awtArticle
  h1_ . toHtml $ articleTitle awtArticle
  p_ . toHtml $ "Created: " <> prettyDate (articleCreation awtArticle)
  p_ . toHtml $ "State: " <> show @Text (articleState awtArticle)
  p_ . a_ [urlHref_ aUrl] . toHtml $ renderUrl aUrl
  case articleState awtArticle of
    Archived -> Form.unreadArticle actionR awtUnread
    Unread -> Form.archiveArticle actionR awtArchive
  Form.editArticle editArticleR awtEdit
  Form.deleteArticle actionR awtDelete

editArticle :: Link -> Id Accesstoken -> LText -> Html ()
editArticle actionR changeArticleTitleAcc aTitle = do
  h1_ "Edit article title"
  Form.changeArticleTitle actionR changeArticleTitleAcc aTitle

newArticle :: Link -> Id Accesstoken -> Html ()
newArticle actionR acc = do
  h2_ "Add a new article"
  Form.newArticle actionR acc

articleList :: Link -> Link -> Link -> [ArticleWithTokens] -> Html ()
articleList showArticleR editArticleR actionR articleWithTokens = do
  h1_ "Your articles"
  div_ $
    traverse_
      (articleItem showArticleR editArticleR actionR)
      articleWithTokens

articleItem :: Link -> Link -> Link -> ArticleWithTokens -> Html ()
articleItem showArticleR editArticleR actionR ArticleWithTokens {..} = do
  article_ [class_ "item"] $ do
    itemHeader
    itemMeta
  where
    itemHeader :: Html ()
    itemHeader =
      div_ [class_ "item-header"] $ do
        span_ [class_ "item-title"] $ do
          Form.showArticle showArticleR awtShow $ articleTitle awtArticle

    itemMeta :: Html ()
    itemMeta =
      div_ [class_ "item-meta"] $ do
        itemMetaInfo
        itemMetaIcons

    itemMetaInfo :: Html ()
    itemMetaInfo = do
      let aCreationDate = prettyDate $ articleCreation awtArticle
      let aUrl = articleUrl awtArticle
      ul_ [class_ "item-meta-info"] $ do
        li_ . time_ [datetime_ aCreationDate] . toHtml $ aCreationDate
        li_ "|"
        li_ . a_ [urlHref_ aUrl] . small_ $ renderHostUrl aUrl

    itemMetaIcons :: Html ()
    itemMetaIcons =
      ul_ [class_ "item-meta-icons"] $ do
        case articleState awtArticle of
          Archived -> li_ $ Form.unreadArticle actionR awtUnread
          Unread -> li_ $ Form.archiveArticle actionR awtArchive
        li_ "|"
        li_ $ Form.editArticle editArticleR awtEdit
        li_ "|"
        li_ $ Form.deleteArticle actionR awtDelete

    renderHostUrl :: Uri -> Html ()
    renderHostUrl = toHtml . URI.render . getDomainHost . unUri

    getDomainHost :: URI.URI -> URI.URI
    getDomainHost url =
      let host = URI.unRText <$> url ^? UL.uriAuthority . _Right . UL.authHost
       in fromMaybe URI.emptyURI $ URI.mkURI =<< host

renderUrl :: Uri -> Text
renderUrl = URI.render . unUri

urlHref_ :: Uri -> Attribute
urlHref_ = href_ . renderUrl

prettyDate :: UTCTime -> Text
prettyDate = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
