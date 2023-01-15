module Test.Ui.Web.Controller.Collection
    ( collectionControllerSpec
    ) where

import Servant.Links (fieldLink)
import Test.Hspec
    ( Spec
    , aroundAll
    , describe
    , it
    )
import Test.Hspec.Wai (shouldRespondWith)
import Web.FormUrlEncoded (urlEncodeAsForm)

import Lib.Ui.Web.Route qualified as Route

import Lib.Ui.Web.Dto.Form (CreateUnlockLinkForm (..))
import Test.Ui.Web.Controller.Shared
    ( accFromResponse
    , linkAsByteString
    , postCreateCollection
    , postForm
    , withTestEnv
    )

collectionControllerSpec :: Spec
collectionControllerSpec = aroundAll withTestEnv $ do
    describe "Lib.Ui.Web.Controller.Collection" $ do
        it "can create a new collection" $ do
            postCreateCollection `shouldRespondWith` 303

        it "can create an unlock link" $ do
            acc <- accFromResponse =<< postCreateCollection
            let form = urlEncodeAsForm $ CreateUnlockLinkForm acc "/" Nothing Nothing
            postForm (linkAsByteString $ fieldLink Route.createUnlockLink) form `shouldRespondWith` 303
