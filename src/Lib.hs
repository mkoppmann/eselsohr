module Lib (startApp) where

import Controller
import Service (initSystem)
import Web.Scotty

dataFolder :: String
dataFolder = "data"

startApp :: IO ()
startApp = do
  initSystem dataFolder
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
