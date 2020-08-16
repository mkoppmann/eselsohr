module Db.CollectionMapping where

import Database.SQLite.Simple (Connection, NamedParam ((:=)), execute, executeNamed, execute_, queryNamed, query_, withConnection)
import Model

sqlCollectionMapping :: (Connection -> IO a) -> IO a
sqlCollectionMapping = withConnection "data/collections.db"

getCollectionPath :: CollectionId -> String
getCollectionPath cId =
  let sCId = show $ getSqliteUUID $ getCollectionId cId
   in "data/" <> sCId <> ".db"

createCollectionsTable :: Connection -> IO ()
createCollectionsTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS collections (id UUID PRIMARY KEY NOT NULL, accesstoken UUID NOT NULL)"

getCollectionMapping :: Accesstoken -> Connection -> IO [CollectionMapping]
getCollectionMapping acc conn = queryNamed conn "SELECT * FROM collections WHERE accesstoken = :accesstoken" [":accesstoken" := acc]

persistNewCollection :: CollectionMapping -> Connection -> IO ()
persistNewCollection collMap conn = execute conn "INSERT INTO collections (id, accesstoken) VALUES (?,?)" collMap

getCollectionMappings :: Connection -> IO [CollectionMapping]
getCollectionMappings conn = query_ conn "SELECT * FROM collections"

getAccesstoken :: CollectionId -> Connection -> IO [CollectionMapping]
getAccesstoken collId conn = queryNamed conn "SELECT * FROM collections WHERE id = :id" [":id" := collId]

deleteCollectionEntry :: CollectionId -> Connection -> IO ()
deleteCollectionEntry collId conn = executeNamed conn "DELETE FROM collections WHERE id = :id" [":id" := collId]
