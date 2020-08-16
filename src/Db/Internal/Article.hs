module Db.Internal.Article where

import Database.SQLite.Simple (Connection, NamedParam ((:=)), execute, executeNamed, execute_, queryNamed, query_, withConnection)
import Model

directInitArticlesTable :: String -> IO ()
directInitArticlesTable path = withConnection path directCreateArticlesTable

directCreateArticlesTable :: Connection -> IO ()
directCreateArticlesTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS articles (id UUID PRIMARY KEY NOT NULL, title TEXT NOT NULL, href TEXT NOT NULL, created_at TIMESTAMP NOT NULL)"

directGetArticles :: Connection -> IO [Article]
directGetArticles conn = query_ conn "SELECT * FROM articles"

directGetArticle :: SqliteUUID -> Connection -> IO [Article]
directGetArticle aId conn = queryNamed conn "SELECT * FROM articles WHERE id = :id" [":id" := aId]

directInsertArticle :: Article -> Connection -> IO ()
directInsertArticle article conn = execute conn "INSERT INTO articles (id, title, href, created_at) VALUES (?,?,?,?)" article

directPatchArticle :: SqliteUUID -> LText -> Connection -> IO ()
directPatchArticle aId title conn = executeNamed conn "UPDATE articles SET title = :title WHERE id = :id" [":title" := title, ":id" := aId]

directDeleteArticle :: SqliteUUID -> Connection -> IO ()
directDeleteArticle aId conn = executeNamed conn "DELETE FROM articles WHERE id = :id" [":id" := aId]
