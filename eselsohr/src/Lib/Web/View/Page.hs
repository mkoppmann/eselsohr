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
import Lib.Core.Domain.Accesstoken (Accesstoken, Revocable)
import Lib.Core.Domain.Article (Article, ArticleState (..))
import qualified Lib.Core.Domain.Article as Article
import Lib.Core.Domain.Capability (Capability (..))
import Lib.Core.Domain.ExpirationDate (ExpirationDate, expDateToText)
import Lib.Core.Domain.Frontend (ResourceOverviewAccess (..), ShowArticleAccess (..), ShowArticlesAccess (..))
import Lib.Core.Domain.Uri (Uri, render, unUri)
import qualified Lib.Web.Route.Common as Route
import qualified Lib.Web.View.Form as Form
import Lucid
import Lucid.Servant (linkAbsHref_)
import Servant.Links (Link)
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
  p_ "You have to provide a valid token to perform this action."

root :: Html ()
root = do
  h1_ "Welcome to Eselsohr"
  p_
    "Eselsohr is a service focused on simplicity.\
    \ Save web articles and consume them later.\
    \ Start your collection by clicking on the button."
  Form.createCollection

resourceOverview ::
  Foldable t =>
  Accesstoken ->
  (ExpirationDate, ExpirationDate) ->
  ResourceOverviewAccess ->
  t (Capability, Revocable) ->
  Html ()
resourceOverview viewAcc dates roAcc activeLinks = do
  h1_ "Collection Main Page"
  p_
    "Welcome to the main page of this collection.\
    \ You can unlock your article list here, as well as accessing the sharing\
    \ menu."
  p_
    "Bookmark this page for quick access or store it in your password manager.\
    \ If you loose access to this page, you can cannot access it again. Treat\
    \ the link like a password.\
    \ Do not share it with anyone."

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
    Just acc -> Form.createGetArticlesCap dates acc $ collectionMainLink viewAcc
  where
    renderActiveLinks :: (Capability, Revocable) -> Html ()
    renderActiveLinks (Capability mPetname mExpDate _, (accId, delAcc)) =
      li_ $ do
        let petname = fromMaybe (show accId) mPetname
            expDate = maybe "Never" (toHtml . expDateToText) mExpDate
        activeLinkA accId petname
        small_ $ "Expires on: " <> expDate
        Form.deleteUnlockLink delAcc $ collectionMainLink viewAcc

    activeLinkA :: Accesstoken -> Text -> Html ()
    activeLinkA acc = a_ [linkAbsHref_ . Route.listArticlesR $ Just acc] . toHtml

    collectionMainLink :: Accesstoken -> Link
    collectionMainLink = Route.collectionMainR . Just

    noActiveLinks :: Html ()
    noActiveLinks = p_ "You have no active access links for this collection"

articleList :: Accesstoken -> ShowArticlesAccess -> Html ()
articleList viewAcc sasAcc = do
  case sasAccCreateArticle sasAcc of
    Nothing -> pass
    Just acc -> newArticle acc . Route.listArticlesR $ Just viewAcc
  h1_ "Your articles"
  div_ $
    traverse_ (`articleItem` viewAcc) $ sasAccArticleAccs sasAcc

newArticle :: Accesstoken -> Link -> Html ()
newArticle acc goto = do
  h2_ "Add a new article"
  Form.createArticle acc goto

articleItem :: (Article, ShowArticleAccess) -> Accesstoken -> Html ()
articleItem (art, saAcc) viewAcc =
  article_ [class_ "item"] $ do
    itemHeader
    itemMeta
  where
    itemHeader :: Html ()
    itemHeader =
      div_ [class_ "item-header"] $ do
        span_ [class_ "item-title"] $ do
          a_
            [linkAbsHref_ . Route.showArticleR . Just $ saAccShowArticle saAcc]
            . toHtml
            $ Article.title art

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
              Nothing -> pass
              Just acc -> li_ $ Form.unreadArticle acc listArticlesLink
          Unread ->
            case saAccArchiveArticle saAcc of
              Nothing -> pass
              Just acc -> li_ $ Form.archiveArticle acc listArticlesLink
        case saAccChangeArticleTitle saAcc of
          Nothing -> pass
          Just _ -> do
            li_ "|"
            li_ . editArticleA $ saAccShowArticle saAcc
        case saAccDeleteArticle saAcc of
          Nothing -> pass
          Just acc -> do
            li_ "|"
            li_ $ Form.deleteArticle acc listArticlesLink

    listArticlesLink :: Link
    listArticlesLink = Route.listArticlesR $ Just viewAcc

    renderHostUrl :: Uri -> Html ()
    renderHostUrl = toHtml . URI.render . getDomainHost . unUri

    getDomainHost :: URI.URI -> URI.URI
    getDomainHost url =
      let host = URI.unRText <$> url ^? UL.uriAuthority . _Right . UL.authHost
       in fromMaybe URI.emptyURI $ URI.mkURI =<< host

editArticle :: Text -> Accesstoken -> Link -> Html ()
editArticle aTitle acc goto = do
  h1_ "Edit article title"
  Form.changeArticleTitle aTitle acc goto

showArticle :: Article -> ShowArticleAccess -> Html ()
showArticle art saAcc = do
  let aUrl = Article.uri art
      showArtAcc = saAccShowArticle saAcc

  whenJust (saAccGetArticles saAcc) getArticlesA

  h1_ . toHtml $ Article.title art
  p_ . toHtml $ "Created: " <> prettyDate (Article.creation art)
  p_ . toHtml $ "State: " <> show @Text (Article.state art)
  p_ . a_ [urlHref_ aUrl] . toHtml $ render aUrl
  case Article.state art of
    Archived -> case saAccUnreadArticle saAcc of
      Nothing -> pass
      Just acc -> Form.unreadArticle acc $ showArticleLink showArtAcc
    Unread -> case saAccArchiveArticle saAcc of
      Nothing -> pass
      Just acc -> Form.archiveArticle acc $ showArticleLink showArtAcc
  case saAccChangeArticleTitle saAcc of
    Nothing -> pass
    Just _ -> editArticleA showArtAcc
  case saAccDeleteArticle saAcc of
    Nothing -> pass
    Just acc -> case saAccGetArticles saAcc of
      Nothing -> root
      Just gaAcc -> Form.deleteArticle acc $ getArticlesLink gaAcc
  where
    showArticleLink :: Accesstoken -> Link
    showArticleLink = Route.showArticleR . Just

    getArticlesLink :: Accesstoken -> Link
    getArticlesLink = Route.listArticlesR . Just

    getArticlesA :: Accesstoken -> Html ()
    getArticlesA acc = a_ [linkAbsHref_ $ getArticlesLink acc] "Back to overview"

editArticleA :: Accesstoken -> Html ()
editArticleA acc = a_ [linkAbsHref_ . Route.editArticleR $ Just acc] "Edit"

-- Helpers

prettyDate :: UTCTime -> Text
prettyDate = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

urlHref_ :: Uri -> Attribute
urlHref_ = href_ . render
