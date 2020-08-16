module Db.Article where

import Capability
import Db.Internal.Article
import Model

initArticlesTable :: CollectionId -> IO ()
initArticlesTable cId =
  let sCId = show $ getSqliteUUID $ getCollectionId cId
      path = "data/" <> sCId <> ".db"
   in directInitArticlesTable path

getArticles :: Capability QueryArticleCapability -> IO [Article]
getArticles cap =
  let artConn = getQueryArticleConn $ getCap cap
   in artConn directGetArticles

getArticle :: SqliteUUID -> Capability QueryArticleCapability -> IO [Article]
getArticle aId cap =
  let artConn = getQueryArticleConn $ getCap cap
   in artConn $ directGetArticle aId

insertArticle :: Article -> Capability CommandArticleCapability -> IO ()
insertArticle article cap =
  let artConn = getCommandArticleConn $ getCap cap
   in artConn $ directInsertArticle article

patchArticle :: SqliteUUID -> LText -> Capability CommandArticleCapability -> IO ()
patchArticle aId aTitle cap =
  let artConn = getCommandArticleConn $ getCap cap
   in artConn $ directPatchArticle aId aTitle

deleteArticle :: SqliteUUID -> Capability CommandArticleCapability -> IO ()
deleteArticle aId cap =
  let artConn = getCommandArticleConn $ getCap cap
   in artConn $ directDeleteArticle aId
