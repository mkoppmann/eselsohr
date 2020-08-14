module Lib (startApp) where

import Controller
import Database.SQLite.Simple
import Db
import System.Directory
import Web.Scotty

startApp :: IO ()
startApp = do
  createDirectoryIfMissing True "data"
  withConnection "data/collections.db" createCollectionsTable
  withConnection "db.db" createArticlesTable
  scotty 8080 $
    cssController
      >> getArticlesController
      >> showArticleController
      >> editArticleController
      >> createArticleController
      >> postArticleController
      >> createCollectionController
      >> getCollectionsController
      >> showCollectionController
      >> postCollectionController
