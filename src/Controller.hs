module Controller where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text, unpack)
import Data.Time (getCurrentTime)
import Data.UUID (fromText)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (withConnection)
import Db
import Model
import Network.HTTP.Types (status400)
import Routes
import System.Directory (removeFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.HTML.Scalpel as SC
import Text.Read (readMaybe)
import View
import Web.Scotty (ActionM, ScottyM, get, html, param, post, raiseStatus, redirect)

mappingDb :: String
mappingDb = "data/collections.db"

-- SCRAPER

fetchTitle :: Text -> IO (Maybe Text)
fetchTitle url = SC.scrapeURL (unpack url) title
  where
    title :: SC.Scraper Text Text
    title = SC.text "h1"

-- ACTIONS

createNewCollection :: ActionM ()
createNewCollection = do
  collectionId <- liftIO $ coerce nextRandom
  accesstoken <- liftIO $ coerce nextRandom
  let uCollectionId = getSqliteUUID $ getCollectionId collectionId
  let collectionDb = "data/" <> show uCollectionId <> ".db"
  let collMap = CollectionMapping collectionId accesstoken
  liftIO $ withConnection collectionDb createArticlesTable
  liftIO $ withConnection mappingDb $ persistNewCollection collMap
  redirect collectionRoute

getCollectionsAction :: ActionM ()
getCollectionsAction = do
  collMaps <- liftIO $ withConnection mappingDb getCollectionMappings
  html $ renderHtml $ collectionMappingList collMaps

getCollectionAction :: ActionM ()
getCollectionAction = do
  accId <- param "acc"
  maybe invalidUUID (getAndRender . coerce) $ fromText accId
  where
    getAndRender acc = do
      collMap <- liftIO $ withConnection mappingDb $ getCollection acc
      let collId = getSqliteUUID $ getCollectionId $ collectionMappingId $ Prelude.head collMap
      let collectionDb = "data/" <> show collId <> ".db"
      articles <- liftIO $ withConnection collectionDb getArticles
      html $ renderHtml $ articleList articles

deleteCollectionAction :: ActionM ()
deleteCollectionAction = do
  accId <- param "acc"
  maybe invalidUUID (delAndRedir . coerce) $ fromText accId
  where
    delAndRedir acc = do
      collMap <- liftIO $ withConnection mappingDb $ getCollection acc
      let collId = collectionMappingId $ Prelude.head collMap
      let sCollId = show $ getSqliteUUID $ getCollectionId collId
      let collectionDb = "data/" <> sCollId <> ".db"
      liftIO $ withConnection mappingDb $ deleteCollection collId
      liftIO $ removeFile collectionDb
      redirect collectionRoute

getArticlesAction :: ActionM ()
getArticlesAction = do
  articles <- liftIO $ withConnection "db.db" getArticles
  html $ renderHtml $ articleList articles

getArticleAction :: ActionM ()
getArticleAction = do
  pId <- param "id"
  maybe invalidUUID (getAndRender . coerce) $ fromText pId
  where
    getAndRender aId = do
      article <- liftIO $ withConnection "db.db" $ getArticle aId
      html $ renderHtml $ articleDetails $ Prelude.head article

createArticleAction :: ActionM ()
createArticleAction = do
  aHref <- param "href"
  aTitle <- liftIO $ fetchTitle aHref
  aTime <- liftIO $ getCurrentTime
  aId <- liftIO $ nextRandom
  let mTitle = fromMaybe "Empty Title" aTitle
  let article = Article (SqliteUUID aId) mTitle aHref aTime
  liftIO $ withConnection "db.db" $ insertArticle article
  redirect articlesRoute

editArticleAction :: ActionM ()
editArticleAction = do
  pId <- param "id"
  maybe invalidUUID (getAndRender . coerce) $ fromText pId
  where
    getAndRender aId = do
      article <- liftIO $ withConnection "db.db" $ getArticle aId
      html $ renderHtml $ editArticleDetails $ Prelude.head article

patchArticleAction :: ActionM ()
patchArticleAction = do
  pId <- param "id"
  nTitle <- param "title"
  maybe invalidUUID (patchAndRedir nTitle . coerce) $ fromText pId
  where
    patchAndRedir nTitle aId = do
      liftIO $ withConnection "db.db" $ patchArticle aId nTitle
      redirect articlesRoute

deleteArticleAction :: ActionM ()
deleteArticleAction = do
  pId <- param "id"
  maybe invalidUUID (delAndRedir . coerce) $ fromText pId
  where
    delAndRedir aId = do
      liftIO $ withConnection "db.db" $ deleteArticle aId
      redirect articlesRoute

invalidUUID :: ActionM ()
invalidUUID = raiseStatus status400 "Not a valid UUID"

-- CONTROLLER

createCollectionController :: ScottyM ()
createCollectionController = post collectionCreationRoutePattern createNewCollection

getCollectionsController :: ScottyM ()
getCollectionsController = get collectionRoutePattern getCollectionsAction

showCollectionController :: ScottyM ()
showCollectionController = get collectionWithAccesstokenRoutePattern getCollectionAction

postCollectionController :: ScottyM ()
postCollectionController = post collectionWithAccesstokenRoutePattern $ do
  action <- param "action"
  case readMaybe action of
    Just DELETE -> deleteCollectionAction
    _ -> getCollectionsAction

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
