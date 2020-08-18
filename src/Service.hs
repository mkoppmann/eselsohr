module Service where

import Capability
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Db.Article
import Db.CollectionMapping
import Model
import Scraper
import System.Directory (createDirectoryIfMissing, removeFile)

initSystem :: String -> IO ()
initSystem dataFolder = do
  createDirectoryIfMissing True dataFolder
  sqlCollectionMapping createCollectionsTable

createNewCollection :: IO Accesstoken
createNewCollection = do
  collectionId <- coerce nextRandom
  accesstoken <- coerce nextRandom
  let collMap = CollectionMapping collectionId accesstoken
  initArticlesTable collectionId
  sqlCollectionMapping $ persistNewCollection collMap
  return accesstoken

getCollections :: IO [CollectionMapping]
getCollections = sqlCollectionMapping getCollectionMappings

getArticlesForCollection :: Accesstoken -> IO (Maybe [Article])
getArticlesForCollection acc =
  getQueryArticlesCap acc
    >>= foldMap getArticles
    >>= (return . pure)

getCollectionMappingService :: Accesstoken -> IO (Maybe CollectionMapping)
getCollectionMappingService acc =
  sqlCollectionMapping (getCollectionMapping acc)
    >>= (return . viaNonEmpty head)

deleteCollection :: CollectionId -> IO ()
deleteCollection cId = do
  sqlCollectionMapping $ deleteCollectionEntry cId
  removeFile $ getCollectionPath cId

getArticleFromCollection :: Accesstoken -> SqliteUUID -> IO (Maybe Article)
getArticleFromCollection acc aId =
  getQueryArticlesCap acc
    >>= foldMap (getArticle aId)
    >>= (return . viaNonEmpty head)

createArticle :: Accesstoken -> LText -> IO ()
createArticle acc aHref = do
  article <- buildArticle aHref
  getCommandArticlesCap acc >>= foldMap (insertArticle article)

editArticle :: Accesstoken -> SqliteUUID -> LText -> IO ()
editArticle acc aId aTitle =
  getCommandArticlesCap acc
    >>= foldMap (patchArticle aId aTitle)

deleteArticleService :: Accesstoken -> SqliteUUID -> IO ()
deleteArticleService acc aId =
  getCommandArticlesCap acc
    >>= foldMap (deleteArticle aId)

buildArticle :: LText -> IO Article
buildArticle aHref = do
  aId <- coerce nextRandom
  aTitle <- scrapWebsiteWithDefaults aHref
  aCreated <- getCurrentTime
  return $ Article aId aTitle aHref aCreated
