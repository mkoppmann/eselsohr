module Lib.Web.View.Page
  ( notAuthorized,
    invalidToken,
    root,
    resourceOverview,
    articleList,
    showArticle,
    editArticle,
  )
where

import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Lens.Micro ((^?), _Right)
import Lib.Core.Domain (Accesstoken, Article, ArticleState (..), Capability (..), ResourceOverviewAccess (..), Revocable, ShowArticleAccess (..), ShowArticlesAccess (..), Uri (..))
import qualified Lib.Core.Domain.Article as Article
import Lib.Core.Domain.ExpirationDate (ExpirationDate, expDateToText)
import qualified Lib.Web.View.Form as Form
import Lucid
import qualified Text.URI as URI
import qualified Text.URI.Lens as UL

notAuthorized :: Html ()
notAuthorized = do
  h1_ "Not authorized"
  p_ "You must provide an accesstoken to access this page."

invalidToken :: Html ()
invalidToken = do
  h1_ "Invalid token"
  p_
    "The token you’ve provided was not accepted. This can have multiple\
    \ reasons:"
  ul_ $ do
    li_ "The token was not created by this system"
    li_ "The token expired"
    li_ "The token reached its maximum usages"
    li_ "The token was revoked by the person who created it"
  p_ "You have to provide a valid token to perform this action."

root :: Accesstoken -> Html ()
root acc = do
  h1_ "Welcome to Eselsohr"
  p_
    "Eselsohr is a service focused on simplicity.\
    \ Save web articles and consume them later.\
    \ Start your collection by clicking on the button."
  Form.createCollection acc

resourceOverview :: Foldable t => Accesstoken -> (ExpirationDate, ExpirationDate) -> ResourceOverviewAccess -> t (Capability, Revocable) -> Html ()
resourceOverview viewAcc dates roAcc activeLinks = do
  Form.refreshResourceOverview viewAcc
  h1_ "Collection Main Page"
  p_
    "Welcome to the main page of this collection.\
    \ You can unlock your article list here, as well as accessing the sharing\
    \ menu."
  p_
    "Bookmark this page for quick access.\
    \ If you loose access to this page, you can cannot access it again. Treat\
    \ the link like a password.\
    \ Do not share it with anyone.\
    \ If you want to share this collection, use the sharing functionality and\
    \ generate new links for others.\
    \ This has the benefit of you keeping the control, because you can add\
    \ restrictions to those links or revoke them at any time."
  ul_ $ do
    li_ . Form.showShareMenu $ roAccGetSharedActions roAcc

  h2_ "Currently active unlock links"
  if null activeLinks
    then noActiveLinks
    else ul_ $ traverse_ renderActiveLinks activeLinks

  h2_ "Unlock article list"
  p_
    "Here you can generate a new token for accessing your article list.\
    \ They are valid for one month, unless you choose something else."
  case roAccCreateGetArticlesCap roAcc of
    Nothing -> p_ "You do not have the permission to create new access links."
    Just acc -> Form.createGetArticlesCap dates acc
  where
    renderActiveLinks (Capability mPetname mExpDate _, (accId, delAcc)) =
      li_ $ do
        let petname = fromMaybe (show accId) mPetname
        let expDate = maybe "Never" (toHtml . expDateToText) mExpDate
        Form.showArticles petname accId
        small_ $ "Expires on: " <> expDate
        Form.deleteUnlockLink delAcc

    noActiveLinks :: Html ()
    noActiveLinks = p_ "You have no active access links for this collection"

articleList :: Accesstoken -> ShowArticlesAccess -> Html ()
articleList viewAcc sasAcc = do
  Form.refreshShowArticles viewAcc
  Form.showPageShare $ sasAccGetSharedArticlesAction sasAcc
  case sasAccCreateArticle sasAcc of
    Nothing -> pure ()
    Just acc -> newArticle acc
  h1_ "Your articles"
  div_ $
    traverse_ articleItem $ sasAccArticleAccs sasAcc

newArticle :: Accesstoken -> Html ()
newArticle acc = do
  h2_ "Add a new article"
  Form.createArticle acc

articleItem :: (Article, ShowArticleAccess) -> Html ()
articleItem (art, saAcc) =
  article_ [class_ "item"] $ do
    itemHeader
    itemMeta
  where
    itemHeader :: Html ()
    itemHeader =
      div_ [class_ "item-header"] $ do
        span_ [class_ "item-title"] $ do
          Form.showArticle (Article.title art) $ saAccShowArticle saAcc

    itemMeta :: Html ()
    itemMeta =
      div_ [class_ "item-meta"] $ do
        itemMetaInfo
        itemMetaIcons

    itemMetaInfo :: Html ()
    itemMetaInfo = do
      let aCreationDate = prettyDate $ Article.creation art
      let aUrl = Article.uri art
      ul_ [class_ "item-meta-info"] $ do
        li_ . time_ [datetime_ aCreationDate] . toHtml $ aCreationDate
        li_ "|"
        li_ . a_ [urlHref_ aUrl] . small_ $ renderHostUrl aUrl

    itemMetaIcons :: Html ()
    itemMetaIcons =
      ul_ [class_ "item-meta-icons"] $ do
        case Article.state art of
          Archived ->
            case saAccUnreadArticle saAcc of
              Nothing -> pure ()
              Just acc -> li_ $ Form.unreadArticle acc
          Unread ->
            case saAccArchiveArticle saAcc of
              Nothing -> pure ()
              Just acc -> li_ $ Form.archiveArticle acc
        case saAccChangeArticleTitle saAcc of
          Nothing -> pure ()
          Just _ -> do
            li_ "|"
            li_ . Form.editArticle "Edit Article" $ saAccShowArticle saAcc
        case saAccDeleteArticle saAcc of
          Nothing -> pure ()
          Just acc -> do
            li_ "|"
            li_ $ Form.deleteArticle acc

    renderHostUrl :: Uri -> Html ()
    renderHostUrl = toHtml . URI.render . getDomainHost . unUri

    getDomainHost :: URI.URI -> URI.URI
    getDomainHost url =
      let host = URI.unRText <$> url ^? UL.uriAuthority . _Right . UL.authHost
       in fromMaybe URI.emptyURI $ URI.mkURI =<< host

editArticle :: Text -> Accesstoken -> Html ()
editArticle aTitle acc = do
  h1_ "Edit article title"
  Form.changeArticleTitle aTitle acc

showArticle :: Article -> ShowArticleAccess -> Html ()
showArticle art saAcc = do
  maybe (pure ()) Form.backToGetArticlesButton $ saAccGetArticles saAcc
  Form.refreshShowArticle $ saAccShowArticle saAcc
  let aUrl = Article.uri art
  h1_ . toHtml $ Article.title art
  p_ . toHtml $ "Created: " <> prettyDate (Article.creation art)
  p_ . toHtml $ "State: " <> show @Text (Article.state art)
  p_ . a_ [urlHref_ aUrl] . toHtml $ renderUrl aUrl
  case Article.state art of
    Archived -> case saAccUnreadArticle saAcc of
      Nothing -> pure ()
      Just acc -> Form.unreadArticle acc
    Unread -> case saAccArchiveArticle saAcc of
      Nothing -> pure ()
      Just acc -> Form.archiveArticle acc
  case saAccChangeArticleTitle saAcc of
    Nothing -> pure ()
    Just _ -> Form.editArticle "Edit" $ saAccShowArticle saAcc
  case saAccDeleteArticle saAcc of
    Nothing -> pure ()
    Just acc -> Form.deleteArticle acc

-- Helpers

prettyDate :: UTCTime -> Text
prettyDate = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

renderUrl :: Uri -> Text
renderUrl = URI.render . unUri

urlHref_ :: Uri -> Attribute
urlHref_ = href_ . renderUrl
