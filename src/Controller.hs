module Controller where

import Data.UUID (toText)
import Model
import Network.HTTP.Types (status404)
import Routes
import Service
import View
import Web.Scotty (ActionM, ScottyM, get, html, param, post, raiseStatus, raw, redirect, setHeader)
import Prelude hiding (get)

-- ACTIONS

rootAction :: ActionM ()
rootAction = html $ renderApp startpage

cssAction :: ActionM ()
cssAction = do
  setHeader "content-type" "text/css; charset=utf-8"
  raw $ encodeUtf8 renderAppStylesheet

createNewCollectionAction :: ActionM ()
createNewCollectionAction = do
  acc <- liftIO createNewCollection
  let accText = toLText . Data.UUID.toText . coerce
  redirect . collectionWithAccesstokenRoute $ accText acc

getCollectionsAction :: ActionM ()
getCollectionsAction = do
  collMaps <- liftIO getCollections
  html $ renderApp $ collectionMappingList collMaps

getArticlesForCollectionAction :: ActionM ()
getArticlesForCollectionAction = do
  pAcc <- param "acc"
  mArticles <- liftIO $ getArticlesForCollection pAcc
  maybe invalidUUID (html . renderApp) $ articleList pAcc <$> mArticles

deleteCollectionAction :: ActionM ()
deleteCollectionAction = do
  pAcc <- param "acc"
  mCollMap <- liftIO $ getCollectionMappingService pAcc
  let mCollId = collectionMappingId <$> mCollMap
  liftIO $ foldMap deleteCollection mCollId
  redirect collectionRoute

getArticleAction :: ActionM ()
getArticleAction = do
  pAcc <- param "acc"
  pId <- param "id"
  mArticle <- liftIO $ getArticleFromCollection pAcc pId
  maybe invalidUUID (html . renderApp) $ articleDetails pAcc <$> mArticle

createArticleAction :: ActionM ()
createArticleAction = do
  pAcc <- param "acc"
  pHref <- param "href"
  liftIO $ createArticle pAcc pHref
  let tAcc = show @LText . getSqliteUUID $ getAccesstoken pAcc
  redirect $ collectionWithAccesstokenRoute tAcc

editArticleAction :: ActionM ()
editArticleAction = do
  pAcc <- param "acc"
  pId <- param "id"
  mArticle <- liftIO $ getArticleFromCollection pAcc pId
  maybe invalidUUID (html . renderApp) $ editArticleDetails pAcc <$> mArticle

patchArticleAction :: ActionM ()
patchArticleAction = do
  pAcc <- param "acc"
  pId <- param "id"
  pTitle <- param "title"
  liftIO $ editArticle pAcc pId pTitle
  let tAcc = show @LText . getSqliteUUID $ getAccesstoken pAcc
  redirect $ collectionWithAccesstokenRoute tAcc

deleteArticleAction :: ActionM ()
deleteArticleAction = do
  pAcc <- param "acc"
  pId <- param "id"
  liftIO $ deleteArticleService pAcc pId
  let tAcc = show @LText . getSqliteUUID $ getAccesstoken pAcc
  redirect $ collectionWithAccesstokenRoute tAcc

invalidUUID :: ActionM ()
invalidUUID = raiseStatus status404 "ID not found"

-- CONTROLLER

rootController :: ScottyM ()
rootController = get rootRoutePattern rootAction

cssController :: ScottyM ()
cssController = get cssRoutePattern cssAction

createCollectionController :: ScottyM ()
createCollectionController = post collectionRoutePattern createNewCollectionAction

getCollectionsController :: ScottyM ()
getCollectionsController = get collectionRoutePattern getCollectionsAction

showCollectionController :: ScottyM ()
showCollectionController = get collectionWithAccesstokenRoutePattern getArticlesForCollectionAction

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
getArticlesController = get articlesRoutePattern getArticlesForCollectionAction
