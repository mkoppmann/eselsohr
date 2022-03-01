module Test.App.Command
  ( commandMockedSpecs
  ) where

import qualified Data.Map.Strict                                     as Map

import           Test.Hspec                                           ( Expectation
                                                                      , Spec
                                                                      , describe
                                                                      , it
                                                                      )

import qualified Lib.App.Command                                     as Command
import qualified Lib.Domain.Article                                  as Art
import qualified Lib.Domain.ArticleList                              as ArtList
import qualified Lib.Domain.Authorization                            as Authz
import qualified Lib.Domain.CapabilityList                           as CapList
import qualified Lib.Domain.Repo.ArticleList                         as ArtRepo
import qualified Lib.Domain.Repo.CapabilityList                      as CapRepo

import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.ArticleList                               ( ArticleList )
import           Lib.Domain.Capability                                ( Capability )
import qualified Lib.Domain.Capability                               as Cap
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Test.Domain.Shared                                   ( getRandomId
                                                                      , getUri
                                                                      , objRefWithAllArticlePerms
                                                                      , objRefWithAllArticlesPerms
                                                                      , objRefWithAllOverviewPerms
                                                                      , objRefWithNoArticlePerms
                                                                      , objRefWithNoArticlesPerms
                                                                      , objRefWithNoOverviewPerms
                                                                      )
import           Test.Infra.Monad                                     ( TestAppEnv )
import           Test.TestAssert                                      ( defaultTestEnv
                                                                      , failsWithEither
                                                                      , runTestApp
                                                                      , satisfies
                                                                      )

commandMockedSpecs :: Spec
commandMockedSpecs = describe "Lib.App.Command" $ do

  describe "Article: " $ do
    it "can create an article with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      let uri     = getUri "http://www.example.org"
          objRef  = objRefWithAllArticlesPerms
          command = Command.CreateArticle { .. }
      void . runTestApp env $ Command.createArticle command
      env & ArtRepo.loadAll colId `satisfies` ((== 1) . Map.size . ArtList.toMap)

    it "fails when creating an article with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      let uri     = getUri "http://www.example.org"
          objRef  = objRefWithNoArticlesPerms
          command = Command.CreateArticle { .. }
      env & Command.createArticle command `failsWithEither` Authz.unauthorized

    it "can change an article’s title with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- addDefaultArticle env colId
      let title   = "New title"
          objRef  = objRefWithAllArticlesPerms
          command = Command.ChangeArticleTitle { .. }
      void . runTestApp env $ Command.changeArticleTitle command
      env & ArtRepo.loadAll colId `satisfies` articleWhere artId ((==) title . toText . Art.title)

    it "fails when changing an article’s title with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- getRandomId
      let title   = "New title"
          objRef  = objRefWithNoArticlesPerms
          command = Command.ChangeArticleTitle { .. }
      env & Command.changeArticleTitle command `failsWithEither` Authz.unauthorized

    it "can mark an article as read with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- addDefaultArticle env colId
      let objRef  = objRefWithAllArticlesPerms
          command = Command.MarkArticleAsRead { .. }
      void . runTestApp env $ Command.markArticleAsRead command
      env & ArtRepo.loadAll colId `satisfies` articleWhere artId ((==) Art.Read . Art.state)

    it "fails when marking an article as read with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- getRandomId
      let objRef  = objRefWithNoArticlesPerms
          command = Command.MarkArticleAsRead { .. }
      env & Command.markArticleAsRead command `failsWithEither` Authz.unauthorized

    it "can mark an article as unread with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- addDefaultArticle env colId
      let objRef  = objRefWithAllArticlesPerms
          command = Command.MarkArticleAsUnread { .. }
      void . runTestApp env $ Command.markArticleAsUnread command
      env & ArtRepo.loadAll colId `satisfies` articleWhere artId ((==) Art.Unread . Art.state)

    it "fails when marking an article as unread with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- getRandomId
      let objRef  = objRefWithNoArticlesPerms
          command = Command.MarkArticleAsUnread { .. }
      env & Command.markArticleAsUnread command `failsWithEither` Authz.unauthorized

    it "can delete an article with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- addDefaultArticle env colId
      let objRef  = objRefWithAllArticlesPerms
          command = Command.DeleteArticle { .. }
      void . runTestApp env $ Command.deleteArticle command
      let predicate = either (const True) (const False) . ArtList.lookup artId
      env & ArtRepo.loadAll colId `satisfies` predicate

    it "fails when deleting an article with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- getRandomId
      let objRef  = objRefWithNoArticlesPerms
          command = Command.DeleteArticle { .. }
      env & Command.deleteArticle command `failsWithEither` Authz.unauthorized

  describe "Capability: " $ do

    it "can create unlock link with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      void $ addUnlockLink env colId
      shouldHaveThatMuchCapabilities colId 1 env

    it "fails when creating an unlock link with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      let mPetname = Nothing
          mExpDate = Nothing
          objRef   = objRefWithNoOverviewPerms
          command  = Command.CreateUnlockLink { .. }
      env & Command.createUnlockLink command `failsWithEither` Authz.unauthorized

    it "can delete an unlock link with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      capId <- addUnlockLink env colId
      let objRef  = objRefWithAllOverviewPerms
          command = Command.DeleteUnlockLink { .. }
      void . runTestApp env $ Command.deleteUnlockLink command
      let predicate = either (const True) (const False) . CapList.lookup capId
      env & CapRepo.loadAll colId `satisfies` predicate

    it "fails when deleting an unlock link with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      capId <- getRandomId
      let objRef  = objRefWithNoOverviewPerms
          command = Command.DeleteUnlockLink { .. }
      env & Command.deleteUnlockLink command `failsWithEither` Authz.unauthorized

    it "can add shared unlock link with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      void $ addSharedUnlockLink env colId
      shouldHaveThatMuchCapabilities colId 1 env

    it "fails when adding a shared unlock link with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      let sharedPerms = Cap.mkOverviewPerms True True True True
          mPetname    = Nothing
          mExpDate    = Nothing
          objRef      = objRefWithNoOverviewPerms
          command     = Command.AddShareUnlockLinks { .. }
      env & Command.addShareUnlockLinks command `failsWithEither` Authz.unauthorized

    it "can delete an shared unlock link with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      capId <- addSharedUnlockLink env colId
      let objRef  = objRefWithAllOverviewPerms
          command = Command.DeleteShareUnlockLinks { .. }
      void . runTestApp env $ Command.deleteShareUnlockLinks command
      let predicate = either (const True) (const False) . CapList.lookup capId
      env & CapRepo.loadAll colId `satisfies` predicate

    it "fails when deleting an shared unlock link with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      capId <- addSharedUnlockLink env colId
      let objRef  = objRefWithNoOverviewPerms
          command = Command.DeleteShareUnlockLinks { .. }
      env & Command.deleteShareUnlockLinks command `failsWithEither` Authz.unauthorized

    it "can add shared article list link with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      void $ addSharedArticleListLink env colId
      shouldHaveThatMuchCapabilities colId 1 env

    it "fails when adding a shared article list link with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      let sharedPerms = Cap.mkArticlesPerms True True True True True True
          mPetname    = Nothing
          mExpDate    = Nothing
          objRef      = objRefWithNoArticlesPerms
          command     = Command.AddShareArticleList { .. }
      env & Command.addShareArticleList command `failsWithEither` Authz.unauthorized

    it "can delete a shared article list link with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      capId <- addSharedArticleListLink env colId
      let objRef  = objRefWithAllArticlesPerms
          command = Command.DeleteShareArticleList { .. }
      void . runTestApp env $ Command.deleteShareArticleList command
      let predicate = either (const True) (const False) . CapList.lookup capId
      env & CapRepo.loadAll colId `satisfies` predicate

    it "fails when deleting a shared article list link with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      capId <- addSharedArticleListLink env colId
      let objRef  = objRefWithNoArticlesPerms
          command = Command.DeleteShareArticleList { .. }
      env & Command.deleteShareArticleList command `failsWithEither` Authz.unauthorized

    it "can add shared article link with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- getRandomId
      void $ addSharedArticleLink env colId artId
      shouldHaveThatMuchCapabilities colId 1 env

    it "fails when adding a shared article link with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- getRandomId
      let sharedPerms = Cap.mkArticlePerms artId True True True True True
          mPetname    = Nothing
          mExpDate    = Nothing
          objRef      = objRefWithNoArticlePerms artId
          command     = Command.AddShareArticle { .. }
      env & Command.addShareArticle command `failsWithEither` Authz.unauthorized

    it "can delete a shared article link with sufficient permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- getRandomId
      capId <- addSharedArticleLink env colId artId
      let objRef  = objRefWithAllArticlePerms artId
          command = Command.DeleteShareArticle { .. }
      void . runTestApp env $ Command.deleteShareArticle command
      let predicate = either (const True) (const False) . CapList.lookup capId
      env & CapRepo.loadAll colId `satisfies` predicate

    it "fails when deleting a shared article link with insufficent permissions" $ do
      env   <- defaultTestEnv
      colId <- getRandomId
      artId <- getRandomId
      capId <- addSharedArticleLink env colId artId
      let objRef  = objRefWithNoArticlePerms artId
          command = Command.DeleteShareArticle { .. }
      env & Command.deleteShareArticle command `failsWithEither` Authz.unauthorized

  describe "Collection: " $ do

    it "can create a new collection" $ do
      env             <- defaultTestEnv
      (colId, _capId) <- runTestApp env Command.createCollection
      shouldHaveThatMuchCapabilities colId 1 env

    it "creating multiple collections don’t overwrite each other" $ do
      env              <- defaultTestEnv
      (colId1, _capId) <- runTestApp env Command.createCollection
      (colId2, _capId) <- runTestApp env Command.createCollection
      shouldHaveThatMuchCapabilities colId1 1 env
      shouldHaveThatMuchCapabilities colId2 1 env

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

addDefaultArticle :: TestAppEnv -> Id Collection -> IO (Id Article)
addDefaultArticle env colId = do
  let uri     = getUri "http://www.example.org"
      objRef  = objRefWithAllArticlesPerms
      command = Command.CreateArticle { .. }
  result <- runTestApp env $ Command.createArticle command
  either displayError pure result

articleWhere :: Id Article -> (Article -> Bool) -> ArticleList -> Bool
articleWhere artId predicate = either (const False) predicate . ArtList.lookup artId

addUnlockLink :: TestAppEnv -> Id Collection -> IO (Id Capability)
addUnlockLink env colId = do
  let mPetname = Nothing
      mExpDate = Nothing
      objRef   = objRefWithAllOverviewPerms
      command  = Command.CreateUnlockLink { .. }
  result <- runTestApp env $ Command.createUnlockLink command
  either displayError pure result

addSharedUnlockLink :: TestAppEnv -> Id Collection -> IO (Id Capability)
addSharedUnlockLink env colId = do
  let sharedPerms = Cap.mkOverviewPerms True True True True
      mPetname    = Nothing
      mExpDate    = Nothing
      objRef      = objRefWithAllOverviewPerms
      command     = Command.AddShareUnlockLinks { .. }
  result <- runTestApp env $ Command.addShareUnlockLinks command
  either displayError pure result

addSharedArticleListLink :: TestAppEnv -> Id Collection -> IO (Id Capability)
addSharedArticleListLink env colId = do
  let sharedPerms = Cap.mkArticlesPerms True True True True True True
      mPetname    = Nothing
      mExpDate    = Nothing
      objRef      = objRefWithAllArticlesPerms
      command     = Command.AddShareArticleList { .. }
  result <- runTestApp env $ Command.addShareArticleList command
  either displayError pure result

addSharedArticleLink :: TestAppEnv -> Id Collection -> Id Article -> IO (Id Capability)
addSharedArticleLink env colId artId = do
  let sharedPerms = Cap.mkArticlePerms artId True True True True True
      mPetname    = Nothing
      mExpDate    = Nothing
      objRef      = objRefWithAllArticlePerms artId
      command     = Command.AddShareArticle { .. }
  result <- runTestApp env $ Command.addShareArticle command
  either displayError pure result

displayError :: (Show a) => a -> b
displayError err = error $ "Could not create value: " <> show err

shouldHaveThatMuchCapabilities :: Id Collection -> Int -> TestAppEnv -> Expectation
shouldHaveThatMuchCapabilities colId amount =
  CapRepo.loadAll colId `satisfies` ((== amount) . Map.size . CapList.toMap)
