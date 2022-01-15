module Test.Ui.Web.Controller.ArticleList
  ( articleListControllerSpec
  ) where

import qualified Data.ByteString.Char8                               as B

import           Servant.Links                                        ( fieldLink )
import           Test.Hspec                                           ( Spec
                                                                      , aroundAll
                                                                      , describe
                                                                      , it
                                                                      )
import           Test.Hspec.Wai                                       ( WaiSession
                                                                      , shouldRespondWith
                                                                      )
import           Web.FormUrlEncoded                                   ( urlEncodeAsForm )

import           Lib.Ui.Web.Route                                    as Route

import           Lib.Ui.Web.Dto.Accesstoken                           ( Accesstoken )
import           Lib.Ui.Web.Dto.Form                                  ( CreateArticleForm(..) )
import           Test.Ui.Web.Controller.Shared                        ( accFromPath
                                                                      , accFromResponse
                                                                      , getOverviewPage
                                                                      , linkAsByteString
                                                                      , postCreateCollection
                                                                      , postCreateUnlockLink
                                                                      , postForm
                                                                      , withTestEnv
                                                                      )

articleListControllerSpec :: Spec
articleListControllerSpec = aroundAll withTestEnv $ do
  describe "Lib.Ui.Web.Controller.ArticleList" $ do

    it "can create a new article" $ do
      acc <- articleListAccesstoken
      let form = urlEncodeAsForm $ CreateArticleForm acc "/" "http://127.0.0.1:6980/"
      postForm (linkAsByteString $ fieldLink Route.createArticle) form `shouldRespondWith` 303

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

articleListAccesstoken :: WaiSession st Accesstoken
articleListAccesstoken = do
  overviewAcc <- accFromResponse =<< postCreateCollection
  void $ postCreateUnlockLink overviewAcc
  overviewRes <- getOverviewPage overviewAcc
  accFromPath $ extractAcc overviewRes
 where
  extractAcc :: ByteString -> ByteString
  extractAcc = B.takeWhile (/= '"') . snd . B.breakSubstring articleListRoute

  articleListRoute :: ByteString
  articleListRoute = linkAsByteString $ fieldLink Route.articleListPage Nothing
