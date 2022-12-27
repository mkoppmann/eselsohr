module Test.Integration
    ( integrations
    ) where

import Test.Tasty
    ( TestTree
    , testGroup
    )
import Test.Tasty.Hspec (testSpec)

import Test.Ui.Web.Controller.ArticleList (articleListControllerSpec)
import Test.Ui.Web.Controller.Collection (collectionControllerSpec)
import Test.Ui.Web.Controller.Static (staticControllerSpec)

integrations :: IO TestTree
integrations = do
    articleListController <- testSpec "ArticleListControllerSpec" articleListControllerSpec
    collectionController <- testSpec "CollectionControllerSpec" collectionControllerSpec
    staticController <- testSpec "StaticControllerSpec" staticControllerSpec
    pure $ testGroup "Integration tests" [articleListController, collectionController, staticController]
