module Capability
  ( QueryArticleCapability (getQueryArticleConn),
    Capability (getCap),
    getQueryArticlesCap,
    CommandArticleCapability (getCommandArticleConn),
    getCommandArticlesCap,
  )
where

import Database.SQLite.Simple (Connection, withConnection)
import Db.CollectionMapping
import Model

newtype Capability a = Capability {getCap :: a}

type QueryArticleConnection = (Connection -> IO [Article]) -> IO [Article]

type CommandArticleConnection = (Connection -> IO ()) -> IO ()

newtype QueryArticleCapability = QueryArticleCapability {getQueryArticleConn :: QueryArticleConnection}

newtype CommandArticleCapability = CommandArticleCapability {getCommandArticleConn :: CommandArticleConnection}

mappingDb :: String
mappingDb = "data/collections.db"

getQueryArticlesCap :: Accesstoken -> IO (Maybe (Capability QueryArticleCapability))
getQueryArticlesCap acc = do
  collMap <- getMapping acc
  let mCollId = viaNonEmpty head collMap
  case mCollId of
    Nothing -> return Nothing
    Just cId -> do
      let collId = getSqliteUUID $ getCollectionId $ collectionMappingId cId
      let collectionDb = "data/" <> show collId <> ".db"
      return $ pure $ Capability $ QueryArticleCapability $ withConnection collectionDb

getCommandArticlesCap :: Accesstoken -> IO (Maybe (Capability CommandArticleCapability))
getCommandArticlesCap acc = do
  collMap <- getMapping acc
  let mCollId = viaNonEmpty head collMap
  case mCollId of
    Nothing -> return Nothing
    Just cId -> do
      let collId = getSqliteUUID $ getCollectionId $ collectionMappingId cId
      let collectionDb = "data/" <> show collId <> ".db"
      return $ pure $ Capability $ CommandArticleCapability $ withConnection collectionDb

getMapping :: Accesstoken -> IO [CollectionMapping]
getMapping acc = withConnection mappingDb $ getCollectionMapping acc
