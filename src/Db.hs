module Db where

import Data.Text.Lazy (Text)
import Database.SQLite.Simple (Connection, NamedParam ((:=)), execute, executeNamed, execute_, queryNamed, query_)
import Model

createCollectionsTable :: Connection -> IO ()
createCollectionsTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS collections (id UUID PRIMARY KEY NOT NULL, accesstoken UUID NOT NULL)"

persistNewCollection :: CollectionMapping -> Connection -> IO ()
persistNewCollection collMap conn = execute conn "INSERT INTO collections (id, accesstoken) VALUES (?,?)" collMap

getCollectionMappings :: Connection -> IO [CollectionMapping]
getCollectionMappings conn = query_ conn "SELECT * FROM collections"

getCollection :: Accesstoken -> Connection -> IO [CollectionMapping]
getCollection acc conn = queryNamed conn "SELECT * FROM collections WHERE accesstoken = :accesstoken" [":accesstoken" := acc]

getAccesstoken :: CollectionId -> Connection -> IO [CollectionMapping]
getAccesstoken collId conn = queryNamed conn "SELECT * FROM collections WHERE id = :id" [":id" := collId]

deleteCollection :: CollectionId -> Connection -> IO ()
deleteCollection collId conn = executeNamed conn "DELETE FROM collections WHERE id = :id" [":id" := collId]

createArticlesTable :: Connection -> IO ()
createArticlesTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS articles (id UUID PRIMARY KEY NOT NULL, title TEXT NOT NULL, href TEXT NOT NULL, created_at TIMESTAMP NOT NULL)"

deleteArticle :: SqliteUUID -> Connection -> IO ()
deleteArticle aId conn = executeNamed conn "DELETE FROM articles WHERE id = :id" [":id" := aId]

patchArticle :: SqliteUUID -> Text -> Connection -> IO ()
patchArticle aId title conn = executeNamed conn "UPDATE articles SET title = :title WHERE id = :id" [":title" := title, ":id" := aId]

insertArticle :: Article -> Connection -> IO ()
insertArticle article conn = do
  execute conn "INSERT INTO articles (id, title, href, created_at) VALUES (?,?,?,?)" article

getArticles :: Connection -> IO [Article]
getArticles conn = do
  query_ conn "SELECT * FROM articles"

getArticle :: SqliteUUID -> Connection -> IO [Article]
getArticle aId conn = queryNamed conn "SELECT * FROM articles WHERE id = :id" [":id" := aId]
