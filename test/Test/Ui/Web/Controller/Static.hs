module Test.Ui.Web.Controller.Static
    ( staticControllerSpec
    ) where

import qualified Test.Hspec.Wai as Test

import Servant.Links (fieldLink)
import Test.Hspec
    ( Spec
    , aroundAll
    , describe
    , it
    )
import Test.Hspec.Wai
    ( ResponseMatcher (..)
    , shouldRespondWith
    )

import qualified Lib.Ui.Web.Route as Route

import Lib.Ui.Web.Route (linkAsText)
import Test.Ui.Web.Controller.Shared
    ( matchPartial
    , withTestEnv
    )

staticControllerSpec :: Spec
staticControllerSpec = aroundAll withTestEnv $ do
    describe "Lib.Ui.Web.Controller.Static" $ do
        it "returns the startpage with 200 containing Eselsohr" $ do
            Test.get (fromRoute Route.startpage) `shouldRespondWith` 200{matchBody = matchPartial "Eselsohr"}

        it "returns the invalid token page with 200" $ do
            Test.get (fromRoute Route.invalidToken) `shouldRespondWith` 200

        it "returns the stylesheet with 200" $ do
            Test.get (fromRoute Route.stylesheet) `shouldRespondWith` 200
  where
    fromRoute route = encodeUtf8 . linkAsText $ fieldLink route
