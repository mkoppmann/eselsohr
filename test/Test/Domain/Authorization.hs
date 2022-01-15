module Test.Domain.Authorization
  ( authorizationSpec
  ) where

import           Test.Hspec                                           ( Spec
                                                                      , SpecWith
                                                                      , describe
                                                                      , it
                                                                      , shouldBe
                                                                      , shouldSatisfy
                                                                      )

import qualified Lib.Domain.Authorization                            as Authz
import qualified Lib.Domain.Capability                               as Cap

import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.Capability                                ( ArticlePerms
                                                                      , ArticlesPerms
                                                                      , ObjectReference
                                                                      , OverviewPerms
                                                                      )
import           Lib.Domain.Error                                     ( AppErrorType )
import           Lib.Domain.Id                                        ( Id )
import           Test.Domain.Shared                                   ( getRandomId )

authorizationSpec :: Spec
authorizationSpec = describe "Lib.Domain.Authorization" $ do

  -- Unlock link permissions

  let viewUnlockLinksPermSuccess = Cap.OverviewPerms { viewUnlockLinksPerm   = Just Cap.ViewUnlockLinks
                                                     , createUnlockLinksPerm = Nothing
                                                     , deleteUnlockLinksPerm = Nothing
                                                     , shareUnlockLinksPerm  = Nothing
                                                     }
      viewUnlockLinksPermFail = Cap.OverviewPerms { viewUnlockLinksPerm   = Nothing
                                                  , createUnlockLinksPerm = Just Cap.CreateUnlockLinks
                                                  , deleteUnlockLinksPerm = Just Cap.DeleteUnlockLinks
                                                  , shareUnlockLinksPerm  = Just Cap.ShareUnlockLinks
                                                  }
  overviewPermsTest "ViewUnlockLinksPerm" viewUnlockLinksPermSuccess viewUnlockLinksPermFail Authz.canViewUnlockLinks

  let createUnlockLinksPermSuccess = Cap.OverviewPerms { viewUnlockLinksPerm   = Nothing
                                                       , createUnlockLinksPerm = Just Cap.CreateUnlockLinks
                                                       , deleteUnlockLinksPerm = Nothing
                                                       , shareUnlockLinksPerm  = Nothing
                                                       }
      createUnlockLinksPermFail = Cap.OverviewPerms { viewUnlockLinksPerm   = Just Cap.ViewUnlockLinks
                                                    , createUnlockLinksPerm = Nothing
                                                    , deleteUnlockLinksPerm = Just Cap.DeleteUnlockLinks
                                                    , shareUnlockLinksPerm  = Just Cap.ShareUnlockLinks
                                                    }
  overviewPermsTest "CreateUnlockLinksPerm"
                    createUnlockLinksPermSuccess
                    createUnlockLinksPermFail
                    Authz.canCreateUnlockLinks

  let deleteUnlockLinksPermSuccess = Cap.OverviewPerms { viewUnlockLinksPerm   = Nothing
                                                       , createUnlockLinksPerm = Nothing
                                                       , deleteUnlockLinksPerm = Just Cap.DeleteUnlockLinks
                                                       , shareUnlockLinksPerm  = Nothing
                                                       }
      deleteUnlockLinksPermFail = Cap.OverviewPerms { viewUnlockLinksPerm   = Just Cap.ViewUnlockLinks
                                                    , createUnlockLinksPerm = Just Cap.CreateUnlockLinks
                                                    , deleteUnlockLinksPerm = Nothing
                                                    , shareUnlockLinksPerm  = Just Cap.ShareUnlockLinks
                                                    }
  overviewPermsTest "DeleteUnlockLinksPerm"
                    deleteUnlockLinksPermSuccess
                    deleteUnlockLinksPermFail
                    Authz.canDeleteUnlockLinks

  let shareUnlockLinksPermSuccess = Cap.OverviewPerms { viewUnlockLinksPerm   = Nothing
                                                      , createUnlockLinksPerm = Nothing
                                                      , deleteUnlockLinksPerm = Nothing
                                                      , shareUnlockLinksPerm  = Just Cap.ShareUnlockLinks
                                                      }
      shareUnlockLinksPermFail = Cap.OverviewPerms { viewUnlockLinksPerm   = Just Cap.ViewUnlockLinks
                                                   , createUnlockLinksPerm = Just Cap.CreateUnlockLinks
                                                   , deleteUnlockLinksPerm = Just Cap.DeleteUnlockLinks
                                                   , shareUnlockLinksPerm  = Nothing
                                                   }
  overviewPermsTest "ShareUnlockLinksPerm"
                    shareUnlockLinksPermSuccess
                    shareUnlockLinksPermFail
                    Authz.canShareUnlockLinks

  -- Article list permissions

  let viewArticlesPermSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                  , createArticlesPerm   = Nothing
                                                  , changeTitlesPerm     = Nothing
                                                  , changeStatesPerm     = Nothing
                                                  , deleteArticlesPerm   = Nothing
                                                  , shareArticleListPerm = Nothing
                                                  }
      viewArticlesPermFail = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                               , createArticlesPerm   = Just Cap.CreateArticles
                                               , changeTitlesPerm     = Just Cap.ChangeTitles
                                               , changeStatesPerm     = Just Cap.ChangeStates
                                               , deleteArticlesPerm   = Just Cap.DeleteArticles
                                               , shareArticleListPerm = Just Cap.ShareArticleList
                                               }
  articlesPermsTest "ViewArticlesPerm" viewArticlesPermSuccess viewArticlesPermFail Authz.canViewArticles

  let createArticlesPermSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                    , createArticlesPerm   = Just Cap.CreateArticles
                                                    , changeTitlesPerm     = Nothing
                                                    , changeStatesPerm     = Nothing
                                                    , deleteArticlesPerm   = Nothing
                                                    , shareArticleListPerm = Nothing
                                                    }
      createArticlesPermFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                 , createArticlesPerm   = Nothing
                                                 , changeTitlesPerm     = Just Cap.ChangeTitles
                                                 , changeStatesPerm     = Just Cap.ChangeStates
                                                 , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                 , shareArticleListPerm = Just Cap.ShareArticleList
                                                 }
  articlesPermsTest "CreateArticlesPerm" createArticlesPermSuccess createArticlesPermFail Authz.canCreateArticles

  let changeArticleTitlesPermSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                         , createArticlesPerm   = Nothing
                                                         , changeTitlesPerm     = Just Cap.ChangeTitles
                                                         , changeStatesPerm     = Nothing
                                                         , deleteArticlesPerm   = Nothing
                                                         , shareArticleListPerm = Nothing
                                                         }
      changeArticleTitlesPermFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                      , createArticlesPerm   = Just Cap.CreateArticles
                                                      , changeTitlesPerm     = Nothing
                                                      , changeStatesPerm     = Just Cap.ChangeStates
                                                      , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                      , shareArticleListPerm = Just Cap.ShareArticleList
                                                      }
  articlesPermsTest "ChangeTitlesPerm" changeArticleTitlesPermSuccess changeArticleTitlesPermFail Authz.canChangeTitles

  let changeArticleStatesPermSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                         , createArticlesPerm   = Nothing
                                                         , changeTitlesPerm     = Nothing
                                                         , changeStatesPerm     = Just Cap.ChangeStates
                                                         , deleteArticlesPerm   = Nothing
                                                         , shareArticleListPerm = Nothing
                                                         }
      changeArticleStatesPermFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                      , createArticlesPerm   = Just Cap.CreateArticles
                                                      , changeTitlesPerm     = Just Cap.ChangeTitles
                                                      , changeStatesPerm     = Nothing
                                                      , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                      , shareArticleListPerm = Just Cap.ShareArticleList
                                                      }
  articlesPermsTest "ChangeStatesPerm" changeArticleStatesPermSuccess changeArticleStatesPermFail Authz.canChangeStates

  let deleteArticlesPermSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                    , createArticlesPerm   = Nothing
                                                    , changeTitlesPerm     = Nothing
                                                    , changeStatesPerm     = Nothing
                                                    , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                    , shareArticleListPerm = Nothing
                                                    }
      deleteArticlesPermFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                 , createArticlesPerm   = Just Cap.CreateArticles
                                                 , changeTitlesPerm     = Just Cap.ChangeTitles
                                                 , changeStatesPerm     = Just Cap.ChangeStates
                                                 , deleteArticlesPerm   = Nothing
                                                 , shareArticleListPerm = Just Cap.ShareArticleList
                                                 }
  articlesPermsTest "DeleteArticlesPerm" deleteArticlesPermSuccess deleteArticlesPermFail Authz.canDeleteArticles

  let shareArticlesPermSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                   , createArticlesPerm   = Nothing
                                                   , changeTitlesPerm     = Nothing
                                                   , changeStatesPerm     = Nothing
                                                   , deleteArticlesPerm   = Nothing
                                                   , shareArticleListPerm = Just Cap.ShareArticleList
                                                   }
      shareArticlesPermFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                , createArticlesPerm   = Just Cap.CreateArticles
                                                , changeTitlesPerm     = Just Cap.ChangeTitles
                                                , changeStatesPerm     = Just Cap.ChangeStates
                                                , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                , shareArticleListPerm = Nothing
                                                }
  articlesPermsTest "ShareArticleListPerm" shareArticlesPermSuccess shareArticlesPermFail Authz.canShareArticleList

  -- Article permissions

  let viewArticlePermArticleSuccess artId = Cap.ArticlePerms { viewArticlePerm   = Just $ Cap.ViewArticle artId
                                                             , changeTitlePerm   = Nothing
                                                             , changeStatePerm   = Nothing
                                                             , deleteArticlePerm = Nothing
                                                             , shareArticlePerm  = Nothing
                                                             }
      viewArticlePermArticleFail artId = Cap.ArticlePerms { viewArticlePerm   = Nothing
                                                          , changeTitlePerm   = Just $ Cap.ChangeTitle artId
                                                          , changeStatePerm   = Just $ Cap.ChangeState artId
                                                          , deleteArticlePerm = Just $ Cap.DeleteArticle artId
                                                          , shareArticlePerm  = Just $ Cap.ShareArticle artId
                                                          }
      viewArticlePermArticlesSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                         , createArticlesPerm   = Nothing
                                                         , changeTitlesPerm     = Nothing
                                                         , changeStatesPerm     = Nothing
                                                         , deleteArticlesPerm   = Nothing
                                                         , shareArticleListPerm = Nothing
                                                         }
      viewArticlePermArticlesFail = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                      , createArticlesPerm   = Just Cap.CreateArticles
                                                      , changeTitlesPerm     = Just Cap.ChangeTitles
                                                      , changeStatesPerm     = Just Cap.ChangeStates
                                                      , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                      , shareArticleListPerm = Just Cap.ShareArticleList
                                                      }
  articlePermsTest "ViewArticlePerm"
                   viewArticlePermArticleSuccess
                   viewArticlePermArticleFail
                   viewArticlePermArticlesSuccess
                   viewArticlePermArticlesFail
                   Authz.canViewArticle

  let changeTitlePermArticleSuccess artId = Cap.ArticlePerms { viewArticlePerm   = Nothing
                                                             , changeTitlePerm   = Just $ Cap.ChangeTitle artId
                                                             , changeStatePerm   = Nothing
                                                             , deleteArticlePerm = Nothing
                                                             , shareArticlePerm  = Nothing
                                                             }
      changeTitlePermArticleFail artId = Cap.ArticlePerms { viewArticlePerm   = Just $ Cap.ViewArticle artId
                                                          , changeTitlePerm   = Nothing
                                                          , changeStatePerm   = Just $ Cap.ChangeState artId
                                                          , deleteArticlePerm = Just $ Cap.DeleteArticle artId
                                                          , shareArticlePerm  = Just $ Cap.ShareArticle artId
                                                          }
      changeTitlePermArticlesSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                         , createArticlesPerm   = Nothing
                                                         , changeTitlesPerm     = Just Cap.ChangeTitles
                                                         , changeStatesPerm     = Nothing
                                                         , deleteArticlesPerm   = Nothing
                                                         , shareArticleListPerm = Nothing
                                                         }
      changeTitlePermArticlesFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                      , createArticlesPerm   = Just Cap.CreateArticles
                                                      , changeTitlesPerm     = Nothing
                                                      , changeStatesPerm     = Just Cap.ChangeStates
                                                      , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                      , shareArticleListPerm = Just Cap.ShareArticleList
                                                      }
  articlePermsTest "ChangeTitlePerm"
                   changeTitlePermArticleSuccess
                   changeTitlePermArticleFail
                   changeTitlePermArticlesSuccess
                   changeTitlePermArticlesFail
                   Authz.canChangeArticleTitle

  let changeStatePermArticleSuccess artId = Cap.ArticlePerms { viewArticlePerm   = Nothing
                                                             , changeTitlePerm   = Nothing
                                                             , changeStatePerm   = Just $ Cap.ChangeState artId
                                                             , deleteArticlePerm = Nothing
                                                             , shareArticlePerm  = Nothing
                                                             }
      changeStatePermArticleFail artId = Cap.ArticlePerms { viewArticlePerm   = Just $ Cap.ViewArticle artId
                                                          , changeTitlePerm   = Just $ Cap.ChangeTitle artId
                                                          , changeStatePerm   = Nothing
                                                          , deleteArticlePerm = Just $ Cap.DeleteArticle artId
                                                          , shareArticlePerm  = Just $ Cap.ShareArticle artId
                                                          }
      changeStatePermArticlesSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                         , createArticlesPerm   = Nothing
                                                         , changeTitlesPerm     = Nothing
                                                         , changeStatesPerm     = Just Cap.ChangeStates
                                                         , deleteArticlesPerm   = Nothing
                                                         , shareArticleListPerm = Nothing
                                                         }
      changeStatePermArticlesFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                      , createArticlesPerm   = Just Cap.CreateArticles
                                                      , changeTitlesPerm     = Just Cap.ChangeTitles
                                                      , changeStatesPerm     = Nothing
                                                      , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                      , shareArticleListPerm = Just Cap.ShareArticleList
                                                      }
  articlePermsTest "ChangeStatePerm"
                   changeStatePermArticleSuccess
                   changeStatePermArticleFail
                   changeStatePermArticlesSuccess
                   changeStatePermArticlesFail
                   Authz.canChangeArticleState

  let deleteArticlePermArticleSuccess artId = Cap.ArticlePerms { viewArticlePerm   = Nothing
                                                               , changeTitlePerm   = Nothing
                                                               , changeStatePerm   = Nothing
                                                               , deleteArticlePerm = Just $ Cap.DeleteArticle artId
                                                               , shareArticlePerm  = Nothing
                                                               }
      deleteArticlePermArticleFail artId = Cap.ArticlePerms { viewArticlePerm   = Just $ Cap.ViewArticle artId
                                                            , changeTitlePerm   = Just $ Cap.ChangeTitle artId
                                                            , changeStatePerm   = Just $ Cap.ChangeState artId
                                                            , deleteArticlePerm = Nothing
                                                            , shareArticlePerm  = Just $ Cap.ShareArticle artId
                                                            }
      deleteArticlePermArticlesSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                           , createArticlesPerm   = Nothing
                                                           , changeTitlesPerm     = Nothing
                                                           , changeStatesPerm     = Nothing
                                                           , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                           , shareArticleListPerm = Nothing
                                                           }
      deleteArticlePermArticlesFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                        , createArticlesPerm   = Just Cap.CreateArticles
                                                        , changeTitlesPerm     = Just Cap.ChangeTitles
                                                        , changeStatesPerm     = Just Cap.ChangeStates
                                                        , deleteArticlesPerm   = Nothing
                                                        , shareArticleListPerm = Just Cap.ShareArticleList
                                                        }
  articlePermsTest "DeleteArticlePerm"
                   deleteArticlePermArticleSuccess
                   deleteArticlePermArticleFail
                   deleteArticlePermArticlesSuccess
                   deleteArticlePermArticlesFail
                   Authz.canDeleteArticle

  let shareArticlePermArticleSuccess artId = Cap.ArticlePerms { viewArticlePerm   = Nothing
                                                              , changeTitlePerm   = Nothing
                                                              , changeStatePerm   = Nothing
                                                              , deleteArticlePerm = Nothing
                                                              , shareArticlePerm  = Just $ Cap.ShareArticle artId
                                                              }
      shareArticlePermArticleFail artId = Cap.ArticlePerms { viewArticlePerm   = Just $ Cap.ViewArticle artId
                                                           , changeTitlePerm   = Just $ Cap.ChangeTitle artId
                                                           , changeStatePerm   = Just $ Cap.ChangeState artId
                                                           , deleteArticlePerm = Just $ Cap.DeleteArticle artId
                                                           , shareArticlePerm  = Nothing
                                                           }
      shareArticlePermArticlesSuccess = Cap.ArticlesPerms { viewArticlesPerm     = Nothing
                                                          , createArticlesPerm   = Nothing
                                                          , changeTitlesPerm     = Nothing
                                                          , changeStatesPerm     = Nothing
                                                          , deleteArticlesPerm   = Nothing
                                                          , shareArticleListPerm = Just Cap.ShareArticleList
                                                          }
      shareArticlePermArticlesFail = Cap.ArticlesPerms { viewArticlesPerm     = Just Cap.ViewArticles
                                                       , createArticlesPerm   = Just Cap.CreateArticles
                                                       , changeTitlesPerm     = Just Cap.ChangeTitles
                                                       , changeStatesPerm     = Just Cap.ChangeStates
                                                       , deleteArticlesPerm   = Just Cap.DeleteArticles
                                                       , shareArticleListPerm = Nothing
                                                       }
  articlePermsTest "ShareArticlePerm"
                   shareArticlePermArticleSuccess
                   shareArticlePermArticleFail
                   shareArticlePermArticlesSuccess
                   shareArticlePermArticlesFail
                   Authz.canShareArticle

------------------------------------------------------------------------
-- Overview permission util
------------------------------------------------------------------------

overviewPermsTest
  :: (Eq a, Show a)
  => String
  -> OverviewPerms
  -> OverviewPerms
  -> (ObjectReference -> Either AppErrorType a)
  -> SpecWith ()
overviewPermsTest name successPerms failPerms auth = do
  returnOverviewPermWhenGivenAuthorizedOverviewRef name successPerms auth
  returnErrorWhenCheckingOverviewPermsGivenUnauthorizedOverviewRef name failPerms auth
  returnOverviewPermWhenGivenAuthorizedSharedOverviewRef name successPerms auth
  returnErrorWhenCheckingOverviewPermsGivenUnauthorizedSharedOverviewRef name failPerms auth
  returnErrorWhenGivenArticlesRefForOverviewPerm name auth
  returnErrorWhenGivenSharedArticlesRefForOverviewPerm name auth
  returnErrorWhenGivenArticleRefForOverviewPerm name auth
  returnErrorWhenGivenSharedArticleRefForOverviewPerm name auth

returnOverviewPermWhenGivenAuthorizedOverviewRef
  :: (Show a) => String -> OverviewPerms -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnOverviewPermWhenGivenAuthorizedOverviewRef name successPerms auth = do
  it (name <> ": returns " <> name <> " when given an authorized 'OverviewRef'") $ do
    let objRef = Cap.OverviewRef successPerms
        perm   = auth objRef
    perm `shouldSatisfy` isRight

returnErrorWhenCheckingOverviewPermsGivenUnauthorizedOverviewRef
  :: (Eq a, Show a) => String -> OverviewPerms -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenCheckingOverviewPermsGivenUnauthorizedOverviewRef name failPerms auth = do
  it (name <> ": returns a 'notAuthorized' error when given an unauthorized 'OverviewRef'") $ do
    let objRef = Cap.OverviewRef failPerms
        perm   = auth objRef
    perm `shouldBe` Authz.unauthorized

returnOverviewPermWhenGivenAuthorizedSharedOverviewRef
  :: (Show a) => String -> OverviewPerms -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnOverviewPermWhenGivenAuthorizedSharedOverviewRef name successPerms auth = do
  it (name <> ": returns " <> name <> " when given an authorized shared 'OverviewRef'") $ do
    let objRef       = Cap.OverviewRef successPerms
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef
    perm `shouldSatisfy` isRight

returnErrorWhenCheckingOverviewPermsGivenUnauthorizedSharedOverviewRef
  :: (Eq a, Show a) => String -> OverviewPerms -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenCheckingOverviewPermsGivenUnauthorizedSharedOverviewRef name failPerms auth = do
  it (name <> ": returns a 'notAuthorized' error when given an unauthorized shared 'OverviewRef'") $ do
    let objRef       = Cap.OverviewRef failPerms
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenArticlesRefForOverviewPerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenArticlesRefForOverviewPerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'ArticlesRef'") $ do
    let objRef = Cap.defaultArticlesRef
        perm   = auth objRef
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenSharedArticlesRefForOverviewPerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenSharedArticlesRefForOverviewPerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'ArticlesRef'") $ do
    let objRef       = Cap.defaultArticlesRef
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenArticleRefForOverviewPerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenArticleRefForOverviewPerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'ArticleRef'") $ do
    objRef <- defaultArticleRef
    let perm = auth objRef
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenSharedArticleRefForOverviewPerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenSharedArticleRefForOverviewPerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'ArticleRef'") $ do
    objRef <- defaultArticleRef
    let sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef
    perm `shouldBe` Authz.unauthorized

------------------------------------------------------------------------
-- Article list permission util
------------------------------------------------------------------------

articlesPermsTest
  :: (Eq a, Show a)
  => String
  -> ArticlesPerms
  -> ArticlesPerms
  -> (ObjectReference -> Either AppErrorType a)
  -> SpecWith ()
articlesPermsTest name successPerms failPerms auth = do
  returnArticlesPermWhenGivenAuthorizedArticlesRef name successPerms auth
  returnErrorWhenCheckingArticlesPermsGivenUnauthorizedArticlesRef name failPerms auth
  returnArticlesPermWhenGivenAuthorizedSharedArticlesRef name successPerms auth
  returnErrorWhenCheckingArticlesPermsGivenUnauthorizedSharedArticlesRef name failPerms auth
  returnErrorWhenGivenOverviewRefForArticlesPerm name auth
  returnErrorWhenGivenSharedOverviewRefForArticlesPerm name auth
  returnErrorWhenGivenArticleRefForArticlesPerm name auth
  returnErrorWhenGivenSharedArticleRefForArticlesPerm name auth

returnArticlesPermWhenGivenAuthorizedArticlesRef
  :: (Show a) => String -> ArticlesPerms -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnArticlesPermWhenGivenAuthorizedArticlesRef name successPerms auth = do
  it (name <> ": returns " <> name <> " when given an authorized 'ArticlesRef'") $ do
    let objRef = Cap.ArticlesRef successPerms
        perm   = auth objRef
    perm `shouldSatisfy` isRight

returnErrorWhenCheckingArticlesPermsGivenUnauthorizedArticlesRef
  :: (Eq a, Show a) => String -> ArticlesPerms -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenCheckingArticlesPermsGivenUnauthorizedArticlesRef name failPerms auth = do
  it (name <> ": returns a 'notAuthorized' error when given an unauthorized 'ArticlesRef'") $ do
    let objRef = Cap.ArticlesRef failPerms
        perm   = auth objRef
    perm `shouldBe` Authz.unauthorized

returnArticlesPermWhenGivenAuthorizedSharedArticlesRef
  :: (Show a) => String -> ArticlesPerms -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnArticlesPermWhenGivenAuthorizedSharedArticlesRef name successPerms auth = do
  it (name <> ": returns " <> name <> " when given an authorized shared 'ArticlesRef'") $ do
    let objRef       = Cap.ArticlesRef successPerms
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef
    perm `shouldSatisfy` isRight

returnErrorWhenCheckingArticlesPermsGivenUnauthorizedSharedArticlesRef
  :: (Eq a, Show a) => String -> ArticlesPerms -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenCheckingArticlesPermsGivenUnauthorizedSharedArticlesRef name failPerms auth = do
  it (name <> ": returns a 'notAuthorized' error when given an unauthorized shared 'ArticlesRef'") $ do
    let objRef       = Cap.ArticlesRef failPerms
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenOverviewRefForArticlesPerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenOverviewRefForArticlesPerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'OverviewRef'") $ do
    let objRef = Cap.defaultOverviewRef
        perm   = auth objRef
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenSharedOverviewRefForArticlesPerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenSharedOverviewRefForArticlesPerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'OverviewRef'") $ do
    let objRef       = Cap.defaultOverviewRef
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenArticleRefForArticlesPerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenArticleRefForArticlesPerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'ArticleRef'") $ do
    objRef <- defaultArticleRef
    let perm = auth objRef
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenSharedArticleRefForArticlesPerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenSharedArticleRefForArticlesPerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'ArticleRef'") $ do
    objRef <- defaultArticleRef
    let sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef
    perm `shouldBe` Authz.unauthorized

------------------------------------------------------------------------
-- Article permission util
------------------------------------------------------------------------

articlePermsTest
  :: (Eq a, Show a)
  => String
  -> (Id Article -> ArticlePerms)
  -> (Id Article -> ArticlePerms)
  -> ArticlesPerms
  -> ArticlesPerms
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
articlePermsTest name successArticlePerms failArticlePerms successArticlesPerms failArticlesPerms auth = do
  returnArticlePermWhenGivenAuthorizedArticleRef name successArticlePerms auth
  returnErrorWhenCheckingArticlePermsGivenUnauthorizedArticleRef name failArticlePerms auth
  returnErrorWhenCheckingArticlePermsGivenArticleRefForWrongId name successArticlePerms auth
  returnArticlePermWhenGivenAuthorizedSharedArticleRef name successArticlePerms auth
  returnErrorWhenCheckingArticlePermsGivenUnauthorizedSharedArticleRef name failArticlePerms auth
  returnErrorWhenCheckingArticlePermsGivenSharedArticleRefForWrongId name successArticlePerms auth
  returnErrorWhenGivenOverviewRefForArticlePerm name auth
  returnErrorWhenGivenSharedOverviewRefForArticlePerm name auth
  returnArticlePermWhenGivenAuthorizedArticlesRef name successArticlesPerms auth
  returnErrorWhenCheckingArticlePermsGivenUnauthorizedArticlesRef name failArticlesPerms auth
  returnArticlePermWhenGivenAuthorizedSharedArticlesRef name successArticlesPerms auth
  returnErrorWhenCheckingArticlePermsGivenUnauthorizedSharedArticlesRef name failArticlesPerms auth

returnArticlePermWhenGivenAuthorizedArticleRef
  :: Show a
  => String
  -> (Id Article -> ArticlePerms)
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
returnArticlePermWhenGivenAuthorizedArticleRef name successPerms auth =
  it (name <> ": returns " <> name <> " when given an authorized 'ArticleRef'") $ do
    artId <- getRandomId
    let objRef = Cap.ArticleRef artId $ successPerms artId
        perm   = auth objRef artId
    perm `shouldSatisfy` isRight

returnErrorWhenCheckingArticlePermsGivenUnauthorizedArticleRef
  :: (Eq a, Show a)
  => String
  -> (Id Article -> ArticlePerms)
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
returnErrorWhenCheckingArticlePermsGivenUnauthorizedArticleRef name failPerms auth = do
  it (name <> ": returns a 'notAuthorized' error when given an unauthorized 'ArticleRef'") $ do
    artId <- getRandomId
    let objRef = Cap.ArticleRef artId $ failPerms artId
        perm   = auth objRef artId
    perm `shouldBe` Authz.unauthorized

returnErrorWhenCheckingArticlePermsGivenArticleRefForWrongId
  :: (Eq a, Show a)
  => String
  -> (Id Article -> ArticlePerms)
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
returnErrorWhenCheckingArticlePermsGivenArticleRefForWrongId name successPerms auth = do
  it
      (name
      <> ": returns a 'notAuthorized' error when given an 'ArticleRef' with enough permissions but for the wrong article ID"
      )
    $ do
        artId      <- getRandomId
        otherArtId <- getRandomId
        let objRef = Cap.ArticleRef artId $ successPerms artId
            perm   = auth objRef otherArtId
        perm `shouldBe` Authz.unauthorized

returnArticlePermWhenGivenAuthorizedSharedArticleRef
  :: Show a
  => String
  -> (Id Article -> ArticlePerms)
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
returnArticlePermWhenGivenAuthorizedSharedArticleRef name successPerms auth = do
  it (name <> ": returns " <> name <> " when given an authorized shared 'ArticleRef'") $ do
    artId <- getRandomId
    let objRef       = Cap.ArticleRef artId $ successPerms artId
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef artId
    perm `shouldSatisfy` isRight

returnErrorWhenCheckingArticlePermsGivenUnauthorizedSharedArticleRef
  :: (Eq a, Show a)
  => String
  -> (Id Article -> ArticlePerms)
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
returnErrorWhenCheckingArticlePermsGivenUnauthorizedSharedArticleRef name failPerms auth = do
  it (name <> ": returns a 'notAuthorized' error when given an unauthorized shared 'ArticleRef'") $ do
    artId <- getRandomId
    let objRef       = Cap.ArticleRef artId $ failPerms artId
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef artId
    perm `shouldBe` Authz.unauthorized

returnErrorWhenCheckingArticlePermsGivenSharedArticleRefForWrongId
  :: (Eq a, Show a)
  => String
  -> (Id Article -> ArticlePerms)
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
returnErrorWhenCheckingArticlePermsGivenSharedArticleRefForWrongId name successPerms auth = do
  it
      (name
      <> ": returns a 'notAuthorized' error when given an shared 'ArticleRef' with enough permissions but for the wrong article ID"
      )
    $ do
        artId      <- getRandomId
        otherArtId <- getRandomId
        let objRef       = Cap.ArticleRef artId $ successPerms artId
            sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
            perm         = auth sharedObjRef otherArtId
        perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenOverviewRefForArticlePerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Id Article -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenOverviewRefForArticlePerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given an 'OverviewRef'") $ do
    artId <- getRandomId
    let objRef = Cap.defaultOverviewRef
        perm   = auth objRef artId
    perm `shouldBe` Authz.unauthorized

returnErrorWhenGivenSharedOverviewRefForArticlePerm
  :: (Eq a, Show a) => String -> (ObjectReference -> Id Article -> Either AppErrorType a) -> SpecWith ()
returnErrorWhenGivenSharedOverviewRefForArticlePerm name auth = do
  it (name <> ": returns a 'notAuthorized' error when given a shared 'OverviewRef'") $ do
    artId <- getRandomId
    let objRef       = Cap.defaultOverviewRef
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef artId
    perm `shouldBe` Authz.unauthorized

returnArticlePermWhenGivenAuthorizedArticlesRef
  :: Show a => String -> ArticlesPerms -> (ObjectReference -> Id Article -> Either AppErrorType a) -> SpecWith ()
returnArticlePermWhenGivenAuthorizedArticlesRef name successPerms auth =
  it (name <> ": returns " <> name <> " when given an authorized 'ArticlesRef'") $ do
    artId <- getRandomId
    let objRef = Cap.ArticlesRef successPerms
        perm   = auth objRef artId
    perm `shouldSatisfy` isRight

returnErrorWhenCheckingArticlePermsGivenUnauthorizedArticlesRef
  :: (Eq a, Show a)
  => String
  -> ArticlesPerms
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
returnErrorWhenCheckingArticlePermsGivenUnauthorizedArticlesRef name failPerms auth = do
  it (name <> ": returns a 'notAuthorized' error when given an unauthorized 'ArticlesRef'") $ do
    artId <- getRandomId
    let objRef = Cap.ArticlesRef failPerms
        perm   = auth objRef artId
    perm `shouldBe` Authz.unauthorized

returnArticlePermWhenGivenAuthorizedSharedArticlesRef
  :: Show a => String -> ArticlesPerms -> (ObjectReference -> Id Article -> Either AppErrorType a) -> SpecWith ()
returnArticlePermWhenGivenAuthorizedSharedArticlesRef name successPerms auth = do
  it (name <> ": returns " <> name <> " when given an authorized shared 'ArticlesRef'") $ do
    artId <- getRandomId
    let objRef       = Cap.ArticlesRef successPerms
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef artId
    perm `shouldSatisfy` isRight

returnErrorWhenCheckingArticlePermsGivenUnauthorizedSharedArticlesRef
  :: (Eq a, Show a)
  => String
  -> ArticlesPerms
  -> (ObjectReference -> Id Article -> Either AppErrorType a)
  -> SpecWith ()
returnErrorWhenCheckingArticlePermsGivenUnauthorizedSharedArticlesRef name failPerms auth = do
  it (name <> ": returns a 'notAuthorized' error when given an unauthorized shared 'ArticleRef'") $ do
    artId <- getRandomId
    let objRef       = Cap.ArticlesRef failPerms
        sharedObjRef = Cap.SharedRef $ Cap.SharedReference objRef
        perm         = auth sharedObjRef artId
    perm `shouldBe` Authz.unauthorized

defaultArticleRef :: (MonadIO m) => m ObjectReference
defaultArticleRef = do
  artId <- getRandomId
  pure . Cap.ArticleRef artId $ defaultArticlePermissions artId

defaultArticlePermissions :: Id Article -> ArticlePerms
defaultArticlePermissions artId = Cap.ArticlePerms (Just $ Cap.ViewArticle artId)
                                                   (Just $ Cap.ChangeTitle artId)
                                                   (Just $ Cap.ChangeState artId)
                                                   (Just $ Cap.DeleteArticle artId)
                                                   (Just $ Cap.ShareArticle artId)
