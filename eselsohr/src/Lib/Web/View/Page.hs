module Lib.Web.View.Page
  ( notAuthorized
  , invalidToken
  , root
  , collectionOverview
  , articleList
  , showArticle
  , editArticle
  ) where

import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           Lens.Micro                     ( (^?)
                                                , _Right
                                                )
import           Lib.Core.Domain                ( Accesstoken
                                                , Article
                                                , ArticleState(..)
                                                , Capability(..)
                                                , Entity(..)
                                                , Id
                                                , Revocable
                                                , Uri
                                                , expDateToText
                                                , render
                                                , unUri
                                                )
import qualified Lib.Core.Domain.Article       as Article
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( ArticleListData(..)
                                                , CollectionOverviewData(..)
                                                , ViewArticleData(..)
                                                )
import qualified Lib.Web.View.Form             as Form
import           Lucid
import           Lucid.Servant                  ( linkAbsHref_ )
import           Servant                        ( Link
                                                , fieldLink
                                                )
import qualified Text.URI                      as URI
import qualified Text.URI.Lens                 as UL

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

collectionOverview :: CollectionOverviewData -> Html ()
collectionOverview CollectionOverviewData {..} = do
  h1_ "Collection Overview Page"
  p_
    "Welcome to the main page of this collection.\
    \ You can unlock your article list here, as well as access the sharing\
    \ menu."
  p_
    "Bookmark this page for quick access or store it in your password manager.\
    \ If you loose access to this page, you can cannot access it again. Treat\
    \ the link like a password.\
    \ Do not share it with anyone."
  h2_ "Currently active unlock links"
  if null unlockLinks
    then p_ "You have no active access links for this collection"
    else ul_ $ traverse_ renderActiveLinks unlockLinks
  h2_ "Unlock article list"
  p_
    "Here you can generate a new token for accessing your article list.\
    \ They are valid for one month, unless you choose something else."
  if canCreateUnlockLink
    then
      Form.createUnlockLink earliestExpDate defaultExpDate acc
      . fieldLink Route.collectionOverview
      $ Just acc
    else p_ "You do not have the permission to create new access links."
 where
  renderActiveLinks :: (Capability, Revocable) -> Html ()
  renderActiveLinks (Capability _ mPetname mExpDate, (capId, activeAcc)) =
    li_ $ do
      let petname = fromMaybe (show capId) mPetname
          expDate = maybe "Never" (toHtml . expDateToText) mExpDate
      activeLinkA activeAcc petname
      small_ $ "Expires on: " <> expDate
      Form.deleteUnlockLink capId acc $ collectionOverviewLink acc

  activeLinkA :: Accesstoken -> Text -> Html ()
  activeLinkA artListAcc =
    a_ [linkAbsHref_ . fieldLink Route.viewArticles $ Just artListAcc] . toHtml

  collectionOverviewLink :: Accesstoken -> Link
  collectionOverviewLink = fieldLink Route.collectionOverview . Just

articleList :: ArticleListData -> Html ()
articleList ArticleListData {..} = do
  if canCreateArticles
    then newArticle acc . fieldLink Route.viewArticles $ Just acc
    else pass
  h1_ "Your articles"
  div_ . void $ traverse_
    (articleItem canChangeArticleTitle
                 canChangeArticleState
                 canDeleteArticle
                 acc
    )
    articles

newArticle :: Accesstoken -> Link -> Html ()
newArticle acc goto = do
  h2_ "Add a new article"
  Form.createArticle acc goto

articleItem :: Bool -> Bool -> Bool -> Accesstoken -> Entity Article -> Html ()
articleItem canChangeTitle canChangeState canDelete acc (Entity artId art) =
  article_ [class_ "item"] $ do
    itemHeader
    itemMeta
 where
  itemHeader :: Html ()
  itemHeader = div_ [class_ "item-header"] $ do
    span_ [class_ "item-title"] $ do
      a_ [linkAbsHref_ . fieldLink Route.viewArticle artId $ Just acc]
        . toHtml
        $ Article.title art

  itemMeta :: Html ()
  itemMeta = div_ [class_ "item-meta"] $ do
    itemMetaInfo
    itemMetaIcons

  itemMetaInfo :: Html ()
  itemMetaInfo = do
    let aCreationDate = prettyDate $ Article.creation art
    let aUrl          = Article.uri art
    ul_ [class_ "item-meta-info"] $ do
      li_ . time_ [datetime_ aCreationDate] . toHtml $ aCreationDate
      li_ "|"
      li_ . a_ [urlHref_ aUrl] . small_ $ renderHostUrl aUrl

  itemMetaIcons :: Html ()
  itemMetaIcons = ul_ [class_ "item-meta-icons"] $ do
    case Article.state art of
      Archived -> if canChangeState
        then li_ $ Form.unreadArticle artId acc listArticlesLink
        else pass
      Unread -> if canChangeState
        then li_ $ Form.archiveArticle artId acc listArticlesLink
        else pass
    if canChangeTitle
      then do
        li_ "|"
        li_ $ editArticleA artId acc
      else pass
    if canDelete
      then do
        li_ "|"
        li_ $ Form.deleteArticle artId acc listArticlesLink
      else pass

  listArticlesLink :: Link
  listArticlesLink = fieldLink Route.viewArticles $ Just acc

  renderHostUrl :: Uri -> Html ()
  renderHostUrl = toHtml . URI.render . getDomainHost . unUri

  getDomainHost :: URI.URI -> URI.URI
  getDomainHost url =
    let host = URI.unRText <$> url ^? UL.uriAuthority . _Right . UL.authHost
    in  fromMaybe URI.emptyURI $ URI.mkURI =<< host

editArticleA :: Id Article -> Accesstoken -> Html ()
editArticleA artId acc =
  a_ [linkAbsHref_ . fieldLink Route.editArticle artId $ Just acc] "Edit"

showArticle :: ViewArticleData -> Html ()
showArticle ViewArticleData {..} = do
  let (Entity artId art) = article
      aUrl               = Article.uri art

  when canViewArticles getArticlesA

  h1_ . toHtml $ Article.title art
  p_ . toHtml $ "Created: " <> prettyDate (Article.creation art)
  p_ . toHtml $ "State: " <> show @Text (Article.state art)
  p_ . a_ [urlHref_ aUrl] . toHtml $ render aUrl
  case Article.state art of
    Archived ->
      when canChangeArticleState
        $ Form.unreadArticle artId acc
        $ showArticleLink artId
    Unread ->
      when canChangeArticleState
        $ Form.archiveArticle artId acc
        $ showArticleLink artId
  when canChangeArticleTitle $ editArticleA artId acc
  when canDeleteArticle $ if canViewArticles
    then Form.deleteArticle artId acc . fieldLink Route.viewArticles $ Just acc
    else root
 where
  showArticleLink :: Id Article -> Link
  showArticleLink artId = fieldLink Route.viewArticle artId $ Just acc

  getArticlesLink :: Link
  getArticlesLink = fieldLink Route.viewArticles $ Just acc

  getArticlesA :: Html ()
  getArticlesA = a_ [linkAbsHref_ getArticlesLink] "Back to overview"

editArticle :: Id Article -> Text -> Accesstoken -> Link -> Html ()
editArticle artId artTitle acc goto = do
  h1_ "Edit article title"
  Form.changeArticleTitle artId artTitle acc goto

-- Helpers

prettyDate :: UTCTime -> Text
prettyDate = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

urlHref_ :: Uri -> Attribute
urlHref_ = href_ . render
