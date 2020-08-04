module Main where

import Control.Monad.IO.Class
import Data.Time
import Data.Text.Lazy
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

-- DATA
data Article = Article
  { articleId :: SqliteUUID,
    articleTitle :: Text,
    articleHref :: Text,
    articleCreate_at :: UTCTime
  }
  deriving (Eq, Read, Show)

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

startPage :: Text -> H.Html
startPage input = H.docTypeHtml $ do
  H.head $ do
    H.title "My first webpage"
  H.body $ do
    H.h1 "Welcome to my first webpage"
    let hInput = H.toHtml input
    H.p "You said: " <> hInput

articleList :: [Article] -> H.Html
articleList articles = H.docTypeHtml $ do
  H.head $ do
    H.title "My article list"
  H.body $ do
    H.h1 "The mighty article list"
    H.ul $ mapM_ renderArticle articles
    H.h2 "Add a new article"
    newArticleForm
  where
    renderArticle = H.li . H.toHtml . prettyArticle
    prettyDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
    prettyArticle (Article _ title href created) = H.p $ do
      H.toHtml $ prettyDate created <> "| "
      H.a H.! A.href (H.lazyTextValue href) $ H.toHtml title

newArticleForm :: H.Html
newArticleForm = H.form H.! A.action "/articles" H.! A.method "POST" $ do
  H.input H.! A.type_ "text" H.! A.name "title" H.! A.placeholder "Title"
  H.input H.! A.type_ "text" H.! A.name "href" H.! A.placeholder "URL"
  H.input H.! A.type_ "submit" H.! A.name "submit"

-- DATABASE

createTable :: Connection -> IO ()
createTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS articles (id UUID PRIMARY KEY NOT NULL, title TEXT NOT NULL, href TEXT NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL)"

insertArticle :: Text -> Text -> Connection -> IO ()
insertArticle title url conn = do
  uuid <- nextRandom
  time <- getCurrentTime
  let article = Article (SqliteUUID uuid) title url time
  execute conn "INSERT INTO articles (id, title, href, created_at) VALUES (?,?,?,?)" article

getArticles :: Connection -> IO [Article]
getArticles conn = do
  query_ conn "SELECT * FROM articles"

withDbConnection :: (Connection -> IO a) -> IO a
withDbConnection = withConnection "db.db"

-- CONTROLLER

insertArticleController :: ScottyM ()
insertArticleController = get "/:word" $ do
  beam <- param "word" :: ActionM Text
  let title = mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  let url = mconcat ["/", beam]
  liftIO $ withDbConnection $ insertArticle title url
  html $ renderHtml $ startPage beam

postArticleController :: ScottyM ()
postArticleController = post "/articles" $ do
  title <- param "title"
  href <- param "href"
  liftIO $ withDbConnection $ insertArticle title href
  redirect "/articles"

getArticlesController :: ScottyM ()
getArticlesController = get "/articles" $ do
  articles <- liftIO $ withDbConnection getArticles
  html $ renderHtml $ articleList articles

-- MAIN

main :: IO ()
main = do
  withDbConnection createTable
  scotty 8080 $
    getArticlesController >> postArticleController
