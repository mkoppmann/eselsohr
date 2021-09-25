module Lib.Web.View.Page
  ( notAuthorized
  , invalidToken
  , root
  , collectionOverview
  , shareOverviewLink
  , articleList
  , newArticlePage
  , shareArticleListLink
  , showArticle
  , shareArticleLink
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
                                                , ShareArticleLinkData(..)
                                                , ShareArticleListLinkData(..)
                                                , ShareOverviewLinkData(..)
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

navBar :: [Html ()] -> Html ()
navBar links = nav_ . ul_ [class_ "no-bullet"] $ traverse_ li_ links

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
  navBar
    [ when canShareLinks $ do
        a_ [linkAbsHref_ . fieldLink Route.shareCollectionOverview $ Just acc]
           "Share this page"
    ]

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
      articlesLinkA activeAcc petname
      p_ . small_ $ "Expires on: " <> expDate
      Form.deleteUnlockLink capId acc $ collectionOverviewLink acc

shareOverviewLink :: ShareOverviewLinkData -> Html ()
shareOverviewLink ShareOverviewLinkData {..} = do
  navBar [collectionOverviewA acc "Back to overview"]

  h1_ "Collection Overview Sharing Menu"
  Form.createSharedOverviewRef sharingPerms earliestExpDate defaultExpDate acc
    . fieldLink Route.shareCollectionOverview
    $ Just acc
  div_ . void $ traverse_ sharedLinkItem sharedLinks
 where
  sharedLinkItem :: (Capability, Revocable) -> Html ()
  sharedLinkItem (Capability _ mPetname _, (capId, sharedAcc)) = do
    li_ $ do
      let petname = fromMaybe (show sharedAcc) mPetname
      collectionOverviewA sharedAcc petname
      Form.deleteSharedReference capId acc
        . fieldLink Route.shareCollectionOverview
        $ Just acc


collectionOverviewA :: Accesstoken -> Text -> Html ()
collectionOverviewA acc =
  a_ [linkAbsHref_ $ collectionOverviewLink acc] . toHtml

collectionOverviewLink :: Accesstoken -> Link
collectionOverviewLink = fieldLink Route.collectionOverview . Just

articlesLinkA :: Accesstoken -> Text -> Html ()
articlesLinkA artListAcc =
  a_ [linkAbsHref_ . fieldLink Route.viewArticles $ Just artListAcc] . toHtml

articleLinkA :: Id Article -> Accesstoken -> Text -> Html ()
articleLinkA artId artListAcc =
  a_ [linkAbsHref_ . fieldLink Route.viewArticle artId $ Just artListAcc]
    . toHtml

articleList :: ArticleListData -> Html ()
articleList ArticleListData {..} = do
  navBar
    [ when canShareLinks . p_ $ do
        a_ [linkAbsHref_ . fieldLink Route.shareViewArticles $ Just acc]
           "Share this page"
    ]

  when canCreateArticles . newArticle acc $ fieldLink Route.viewArticles
                                                      (Just acc)

  h1_ "Your articles"
  div_ . ul_ [] . void $ traverse_
    (articleItem canChangeArticleTitle
                 canChangeArticleState
                 canDeleteArticle
                 acc
    )
    articles

newArticlePage :: Accesstoken -> Link -> Html ()
newArticlePage acc goto = do
  h1_ "Add a new article"
  Form.createArticle acc goto

newArticle :: Accesstoken -> Link -> Html ()
newArticle acc goto = do
  h2_ "Add a new article"
  Form.createArticle acc goto

shareArticleListLink :: ShareArticleListLinkData -> Html ()
shareArticleListLink ShareArticleListLinkData {..} = do
  navBar [when canViewArticles $ getArticlesA acc]

  h1_ "Article List Sharing Menu"
  Form.createSharedArticlesRef sharingPerms earliestExpDate defaultExpDate acc
    . fieldLink Route.shareViewArticles
    $ Just acc
  div_ . void $ traverse_ sharedLinkItem sharedLinks
 where
  sharedLinkItem :: (Capability, Revocable) -> Html ()
  sharedLinkItem (Capability _ mPetname _, (capId, sharedAcc)) = do
    li_ $ do
      let petname = fromMaybe (show sharedAcc) mPetname
      articlesLinkA sharedAcc petname
      Form.deleteSharedReference capId acc
        . fieldLink Route.shareViewArticles
        $ Just acc

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
      when canChangeTitle . small_ $ editArticleA artId acc "✏️"

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
    when canDelete . li_ $ Form.deleteArticle artId acc listArticlesLink

    case Article.state art of
      Archived -> when canChangeState $ do
        li_ "|"
        li_ $ Form.unreadArticle artId acc listArticlesLink
      Unread -> when canChangeState $ do
        li_ "|"
        li_ $ Form.archiveArticle artId acc listArticlesLink

  listArticlesLink :: Link
  listArticlesLink = fieldLink Route.viewArticles $ Just acc

  renderHostUrl :: Uri -> Html ()
  renderHostUrl = toHtml . URI.render . getDomainHost . unUri

  getDomainHost :: URI.URI -> URI.URI
  getDomainHost url =
    let host = URI.unRText <$> url ^? UL.uriAuthority . _Right . UL.authHost
    in  fromMaybe URI.emptyURI $ URI.mkURI =<< host

editArticleA :: Id Article -> Accesstoken -> Text -> Html ()
editArticleA artId acc =
  a_ [linkAbsHref_ . fieldLink Route.editArticle artId $ Just acc] . toHtml

showArticle :: ViewArticleData -> Html ()
showArticle ViewArticleData {..} = do
  let (Entity artId art) = article
      aUrl               = Article.uri art

  navBar
    [ when canViewArticles $ getArticlesA acc
    , when canShareLinks $ do
      a_ [linkAbsHref_ . fieldLink Route.shareViewArticle artId $ Just acc]
         "Share this page"
    ]

  h1_ . toHtml $ Article.title art
  when canChangeArticleTitle . p_ $ editArticleA artId acc "Edit title"
  p_ . toHtml $ "Created: " <> prettyDate (Article.creation art)
  p_ . toHtml $ "State: " <> show @Text (Article.state art)
  p_ . a_ [urlHref_ aUrl] . toHtml $ render aUrl
  when canDeleteArticle $ if canViewArticles
    then Form.deleteArticle artId acc . fieldLink Route.viewArticles $ Just acc
    else Form.deleteArticle artId acc $ fieldLink Route.startpage
  case Article.state art of
    Archived ->
      when canChangeArticleState
        $ Form.unreadArticle artId acc
        $ showArticleLink artId
    Unread ->
      when canChangeArticleState
        $ Form.archiveArticle artId acc
        $ showArticleLink artId
 where
  showArticleLink :: Id Article -> Link
  showArticleLink artId = fieldLink Route.viewArticle artId $ Just acc

shareArticleLink :: Id Article -> ShareArticleLinkData -> Html ()
shareArticleLink artId ShareArticleLinkData {..} = do
  navBar [when canViewArticle $ getArticleA artId acc]

  h1_ "Article Sharing Menu"
  Form.createSharedArticleRef artId
                              sharingPerms
                              earliestExpDate
                              defaultExpDate
                              acc
    . fieldLink Route.shareViewArticle artId
    $ Just acc
  div_ . void $ traverse_ sharedLinkItem sharedLinks
 where
  sharedLinkItem :: (Capability, Revocable) -> Html ()
  sharedLinkItem (Capability _ mPetname _, (capId, sharedAcc)) = do
    li_ $ do
      let petname = fromMaybe (show sharedAcc) mPetname
      articleLinkA artId sharedAcc petname
      Form.deleteSharedReference capId acc
        . fieldLink Route.shareViewArticle artId
        $ Just acc

getArticlesA :: Accesstoken -> Html ()
getArticlesA acc =
  a_ [linkAbsHref_ . fieldLink Route.viewArticles $ Just acc] "Back to overview"

getArticleA :: Id Article -> Accesstoken -> Html ()
getArticleA artId acc = a_
  [linkAbsHref_ . fieldLink Route.viewArticle artId $ Just acc]
  "Back to article"

editArticle :: Id Article -> Text -> Accesstoken -> Link -> Html ()
editArticle artId artTitle acc goto = do
  h1_ "Edit article title"
  Form.changeArticleTitle artId artTitle acc goto

-- Helpers

prettyDate :: UTCTime -> Text
prettyDate = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

urlHref_ :: Uri -> Attribute
urlHref_ = href_ . render
