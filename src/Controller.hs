module Controller where

import Data.UUID (fromText)
import Model
import Network.HTTP.Types (status404)
import Routes
import Service
import View
import Web.Scotty (ActionM, ScottyM, get, html, param, post, raiseStatus, raw, redirect, setHeader)
import Prelude hiding (get)

-- ACTIONS

cssAction :: ActionM ()
cssAction = do
  let css = encodeUtf8 renderAppStylesheet
  setHeader "content-type" "text/css; charset=utf-8"
  raw css

createNewCollectionAction :: ActionM ()
createNewCollectionAction = do
  liftIO createNewCollection
  redirect collectionRoute

getCollectionsAction :: ActionM ()
getCollectionsAction = do
  collMaps <- liftIO getCollections
  html $ renderApp $ collectionMappingList collMaps

getArticlesForCollectionAction :: ActionM ()
getArticlesForCollectionAction = do
  pAcc <- param "acc"
  let mAcc = coerce <$> fromText pAcc
  mArticles <- liftIO $ foldMap getArticlesForCollection mAcc
  let mHtml = articleList <$> mAcc <*> mArticles
  maybe invalidUUID (html . renderApp) mHtml

deleteCollectionAction :: ActionM ()
deleteCollectionAction = do
  pAcc <- param "acc"
  let mAcc = Accesstoken . SqliteUUID <$> fromText pAcc
  mCollMap <- liftIO $ join <$> mapM getCollectionMappingService mAcc
  let mCollId = collectionMappingId <$> mCollMap
  liftIO $ foldMap deleteCollection mCollId
  redirect collectionRoute

getArticleAction :: ActionM ()
getArticleAction = do
  pAcc <- param "acc"
  pId <- param "id"
  let mAcc = coerce <$> fromText pAcc
  let mId = coerce <$> fromText pId
  mArticle <- liftIO $ fmap join $ sequence $ getArticleFromCollection <$> mAcc <*> mId
  maybe invalidUUID (html . renderApp) $ articleDetails <$> mAcc <*> mArticle

createArticleAction :: ActionM ()
createArticleAction = do
  pAcc <- param "acc"
  pHref <- param "href"
  let mAcc = coerce <$> fromText pAcc
  _ <- liftIO $ sequence $ flip createArticle pHref <$> mAcc
  redirect $ collectionWithAccesstokenRoute $ toLText pAcc

editArticleAction :: ActionM ()
editArticleAction = do
  pAcc <- param "acc"
  pId <- param "id"
  let mAcc = coerce <$> fromText pAcc
  let mId = coerce <$> fromText pId
  mArticle <- liftIO $ fmap join $ sequence $ getArticleFromCollection <$> mAcc <*> mId
  maybe invalidUUID (html . renderApp) $ editArticleDetails <$> mAcc <*> mArticle

patchArticleAction :: ActionM ()
patchArticleAction = do
  pAcc <- param "acc"
  pId <- param "id"
  pTitle <- param "title"
  let mAcc = coerce <$> fromText pAcc
  let mId = coerce <$> fromText pId
  let mTitle = pure pTitle
  _ <- liftIO $ sequence $ editArticle <$> mAcc <*> mId <*> mTitle
  redirect $ collectionWithAccesstokenRoute $ toLText pAcc

deleteArticleAction :: ActionM ()
deleteArticleAction = do
  pAcc <- param "acc"
  pId <- param "id"
  let mAcc = coerce <$> fromText pAcc
  let mId = coerce <$> fromText pId
  _ <- liftIO $ sequence $ deleteArticleService <$> mAcc <*> mId
  redirect $ collectionWithAccesstokenRoute $ toLText pAcc

invalidUUID :: ActionM ()
invalidUUID = raiseStatus status404 "ID not found"

-- CONTROLLER

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
