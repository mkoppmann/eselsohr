module Main where

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text.Lazy
import Data.Time
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Network.HTTP.Types.Status
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.HTML.Scalpel as SC
import Text.Read (readMaybe)
import Web.Scotty

-- DATA
data Article = Article
  { articleId :: SqliteUUID,
    articleTitle :: Text,
    articleHref :: Text,
    articleCreate_at :: UTCTime
  }
  deriving (Eq, Read, Show)

data Actions = PATCH | DELETE deriving (Eq, Read, Show)

newtype SqliteUUID = SqliteUUID {getUUID :: UUID}
  deriving (Eq, Read, Show)

instance FromField SqliteUUID where
  fromField f@(Field (SQLText t) _) =
    case fromText t of
      Just uuid -> Ok $ SqliteUUID uuid
      Nothing -> returnError ConversionFailed f "not a valid UUID"
  fromField f = returnError ConversionFailed f "error converting UUID"

instance ToField SqliteUUID where
  toField = SQLText . toText . getUUID
  {-# INLINE toField #-}

instance FromRow Article where
  fromRow = Article <$> field <*> field <*> field <*> field

instance ToRow Article where
  toRow (Article aID aTitle aHref aCreated) = toRow (aID, aTitle, aHref, aCreated)

-- HTML Views

articleList :: [Article] -> H.Html
articleList articles = H.docTypeHtml $ do
  H.head $ do
    H.title "My article list"
  H.body $ do
    H.h2 "Add a new article"
    newArticleForm
    H.h1 "The mighty article list"
    H.ul $ mapM_ renderArticle articles
  where
    renderArticle = H.li . H.toHtml . prettyArticle
    prettyArticle (Article aId title _ created) = do
      H.p $ do
        H.toHtml $ prettyDate created <> "| "
        H.a H.! A.href (uuidToAttribute articleWithIdRoute aId) $ H.toHtml title
      editArticleForm aId
      deleteArticleForm aId

articleDetails :: Article -> H.Html
articleDetails (Article aId title href created) = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ title <> " – Eselsohr"
  H.body $ do
    H.h1 $ H.toHtml title
    H.p $ H.toHtml $ "Created: " <> prettyDate created
    H.p $ H.a H.! A.href (H.lazyTextValue href) $ H.toHtml href
    editArticleForm aId
    deleteArticleForm aId
    H.p $ H.a H.! A.href (H.lazyTextValue articlesRoute) $ "Back to articles list"

editArticleDetails :: Article -> H.Html
editArticleDetails (Article aId title href _) = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ "Edit – " <> title <> " – Eselsohr"
  H.body $ do
    H.h1 "Edit article"
    H.form H.! A.action (uuidToAttribute articleWithIdRoute aId) H.! A.method "POST" $ do
      H.input H.! A.type_ "text" H.! A.name "title" H.! A.value (H.lazyTextValue title)
      H.p $ H.toHtml href
      H.input H.! A.type_ "hidden" H.! A.name "action" H.! A.value "PATCH"
      H.input H.! A.type_ "submit" H.! A.name "submit" H.! A.value "Save article"
    H.p $ H.a H.! A.href (H.lazyTextValue articlesRoute) $ "Back to articles list"

newArticleForm :: H.Html
newArticleForm = H.form H.! A.action (H.lazyTextValue articlesRoute) H.! A.method "POST" $ do
  H.input H.! A.type_ "text" H.! A.name "href" H.! A.placeholder "URL"
  H.input H.! A.type_ "submit" H.! A.name "submit"

editArticleForm :: SqliteUUID -> H.Html
editArticleForm aId = H.form H.! A.action (uuidToAttribute editArticleWithIdRoute aId) H.! A.method "GET" $ do
  H.input H.! A.type_ "submit" H.! A.name "submit" H.! A.value "Edit article"

deleteArticleForm :: SqliteUUID -> H.Html
deleteArticleForm aId = H.form H.! A.action (uuidToAttribute articleWithIdRoute aId) H.! A.method "POST" $ do
  H.input H.! A.type_ "hidden" H.! A.name "action" H.! A.value "DELETE"
  H.input H.! A.type_ "submit" H.! A.name "submit" H.! A.value "Delete article"

prettyDate :: UTCTime -> Text
prettyDate = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

uuidToAttribute :: (String -> String) -> SqliteUUID -> H.AttributeValue
uuidToAttribute route = H.lazyTextValue . pack . route . toString . getUUID

-- DATABASE

createTable :: Connection -> IO ()
createTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS articles (id UUID PRIMARY KEY NOT NULL, title TEXT NOT NULL, href TEXT NOT NULL, created_at TIMESTAMP NOT NULL)"

deleteArticle :: SqliteUUID -> Connection -> IO ()
deleteArticle aId conn = executeNamed conn "DELETE FROM articles WHERE id = :id" [":id" := aId]

patchArticle :: SqliteUUID -> Text -> Connection -> IO ()
patchArticle aId title conn = executeNamed conn "UPDATE articles SET title = :title WHERE id = :id" [":title" := title, ":id" := aId]

insertArticle :: Text -> Connection -> IO ()
insertArticle url conn = do
  uuid <- nextRandom
  time <- getCurrentTime
  mTitle <- fetchTitle url
  let title = fromMaybe "Empty Title" mTitle
  let article = Article (SqliteUUID uuid) title url time
  execute conn "INSERT INTO articles (id, title, href, created_at) VALUES (?,?,?,?)" article

getArticles :: Connection -> IO [Article]
getArticles conn = do
  query_ conn "SELECT * FROM articles"

getArticle :: SqliteUUID -> Connection -> IO [Article]
getArticle aId conn = queryNamed conn "SELECT * FROM articles WHERE id = :id" [":id" := aId]

withDbConnection :: (Connection -> IO a) -> IO a
withDbConnection = withConnection "db.db"

-- SCRAPER

fetchTitle :: Text -> IO (Maybe Text)
fetchTitle url = SC.scrapeURL (unpack url) title
  where
    title :: SC.Scraper Text Text
    title = SC.text "h1"

-- ACTIONS

getArticlesAction :: ActionM ()
getArticlesAction = do
  articles <- liftIO $ withDbConnection getArticles
  html $ renderHtml $ articleList articles

getArticleAction :: ActionM ()
getArticleAction = do
  pId <- param "id"
  maybe invalidUUID (getAndRender . SqliteUUID) $ fromText pId
  where
    getAndRender aId = do
      article <- liftIO $ withDbConnection $ getArticle aId
      html $ renderHtml $ articleDetails $ Prelude.head article

createArticleAction :: ActionM ()
createArticleAction = do
  href <- param "href"
  liftIO $ withDbConnection $ insertArticle href
  redirect articlesRoute

editArticleAction :: ActionM ()
editArticleAction = do
  pId <- param "id"
  maybe invalidUUID (getAndRender . SqliteUUID) $ fromText pId
  where
    getAndRender aId = do
      article <- liftIO $ withDbConnection $ getArticle aId
      html $ renderHtml $ editArticleDetails $ Prelude.head article

patchArticleAction :: ActionM ()
patchArticleAction = do
  pId <- param "id"
  nTitle <- param "title"
  maybe invalidUUID (patchAndRedir nTitle . SqliteUUID) $ fromText pId
  where
    patchAndRedir nTitle aId = do
      liftIO $ withDbConnection $ patchArticle aId nTitle
      redirect articlesRoute

deleteArticleAction :: ActionM ()
deleteArticleAction = do
  pId <- param "id"
  maybe invalidUUID (delAndRedir . SqliteUUID) $ fromText pId
  where
    delAndRedir aId = do
      liftIO $ withDbConnection $ deleteArticle aId
      redirect articlesRoute

withArticleId :: String -> (SqliteUUID -> b) -> b -> b
withArticleId pId okF failF = maybe failF (okF . SqliteUUID) $ fromString pId

invalidUUID :: ActionM ()
invalidUUID = raiseStatus status400 "Not a valid UUID"

-- CONTROLLER

postArticleController :: ScottyM ()
postArticleController = post articleWithIdRoutePattern $ do
  action <- param "action"
  case readMaybe action of
    Just PATCH -> patchArticleAction
    Just DELETE -> deleteArticleAction
    Nothing -> getArticleAction

editArticleController :: ScottyM ()
editArticleController = get editArticleWithIdRoutePattern editArticleAction

createArticleController :: ScottyM ()
createArticleController = post articlesRoutePattern createArticleAction

showArticleController :: ScottyM ()
showArticleController = get articleWithIdRoutePattern getArticleAction

getArticlesController :: ScottyM ()
getArticlesController = get articlesRoutePattern getArticlesAction

-- ROUTES

articlesRoute :: Text
articlesRoute = "/articles"

articlesRoutePattern :: RoutePattern
articlesRoutePattern = "/articles"

articleWithIdRoute :: (Semigroup a, IsString a) => a -> a
articleWithIdRoute aId = "/article/" <> aId

articleWithIdRoutePattern :: RoutePattern
articleWithIdRoutePattern = "/article/:id"

editArticleWithIdRoute :: (Semigroup a, IsString a) => a -> a
editArticleWithIdRoute aId = "/article/" <> aId <> "/edit"

editArticleWithIdRoutePattern :: RoutePattern
editArticleWithIdRoutePattern = "/article/:id/edit"

-- MAIN

main :: IO ()
main = do
  withDbConnection createTable
  scotty 8080 $
    getArticlesController
      >> showArticleController
      >> editArticleController
      >> createArticleController
      >> postArticleController
