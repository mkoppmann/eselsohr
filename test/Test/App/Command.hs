module Test.App.Command
  ( commandMockedSpecs
  ) where

import           Test.Hspec                                           ( Spec
                                                                      , describe
                                                                      , it
                                                                      )

import qualified Lib.App.Command                                     as Command
import qualified Lib.Domain.Authorization                            as Authz

import           Test.Domain.Shared                                   ( getRandomId
                                                                      , getUri
                                                                      , objRefWithAllArticlesPerms
                                                                      , objRefWithNoArticlesPerms
                                                                      )
import           Test.TestAssert                                      ( defaultTestEnv
                                                                      , failsWithEither
                                                                      , succeeds
                                                                      )

commandMockedSpecs :: Spec
commandMockedSpecs = describe "Lib.App.Command" $ do

  it "can create an article with sufficient permissions" $ do
    env   <- defaultTestEnv
    colId <- getRandomId
    let uri     = getUri "http://www.example.org"
        objRef  = objRefWithAllArticlesPerms
        command = Command.CreateArticle { .. }
    env & succeeds (Command.createArticle command)

  it "fails when creating an article with insufficent permissions" $ do
    env   <- defaultTestEnv
    colId <- getRandomId
    let uri     = getUri "http://www.example.org"
        objRef  = objRefWithNoArticlesPerms
        command = Command.CreateArticle { .. }
    env & Command.createArticle command `failsWithEither` Authz.unauthorized
