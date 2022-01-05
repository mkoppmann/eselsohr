module Test.Domain.Capability
  ( capabilitySpec
  ) where

import           Data.Time.Clock                                      ( UTCTime )
import           Prelude                                       hiding ( id )
import           Test.Hspec                                           ( Spec
                                                                      , describe
                                                                      , it
                                                                      , shouldNotSatisfy
                                                                      , shouldSatisfy
                                                                      )

import qualified Lib.Domain.Capability                               as Capability

import           Lib.Domain.Capability                                ( Capability
                                                                      , ObjectReference
                                                                      )
import           Test.Domain.Shared                                   ( getCurrentTime
                                                                      , getRandomId
                                                                      , objRefWithAllArticlePerms
                                                                      )

capabilitySpec :: Spec
capabilitySpec = describe "Lib.Domain.Capability" $ do

  it "capabilities with same IDs but different values are equal" $ do
    cap1 <- capabilityWithObjRef Capability.defaultOverviewRef
    let cap2 = cap1 { Capability.objectReference = Capability.defaultArticlesRef, Capability.petname = Just "petname" }
    cap1 `shouldSatisfy` (== cap2)

  it "capabilities with different IDs but same values are not equal" $ do
    cap1 <- capabilityWithObjRef Capability.defaultOverviewRef
    cap2 <- capabilityWithObjRef Capability.defaultOverviewRef
    cap1 `shouldSatisfy` (/= cap2)

  it "capabilities with later expiration dates are greater than capabilities with earlier ones" $ do
    date1 <- getCurrentTime
    cap1  <- capabilityWithNewExpDate date1
    date2 <- getCurrentTime
    cap2  <- capabilityWithNewExpDate date2
    cap2 `shouldSatisfy` (> cap1)

  it "'mkCapability' does not accept empty petnames" $ do
    capId <- getRandomId
    let cap = Capability.mkCapability capId Capability.defaultOverviewRef (Just "") Nothing
    Capability.petname cap `shouldNotSatisfy` isJust

  it "'isSharedRef' returns 'True' if given 'SharedRef'" $ do
    let sharedRef = Capability.SharedRef $ Capability.SharedReference Capability.defaultOverviewRef
    sharedRef `shouldSatisfy` Capability.isSharedRef

  it "'isSharedRef' returns 'False' if given 'OverviewRef'" $ do
    let ref = Capability.defaultOverviewRef
    ref `shouldNotSatisfy` Capability.isSharedRef

  it "'isSharedRef' returns 'False' if given 'ArticlesRef'" $ do
    let ref = Capability.defaultArticlesRef
    ref `shouldNotSatisfy` Capability.isSharedRef

  it "'isSharedRef' returns 'False' if given 'ArticleRef'" $ do
    artId <- getRandomId
    let ref = objRefWithAllArticlePerms artId
    ref `shouldNotSatisfy` Capability.isSharedRef

  it "'isSharedOverviewRef' returns 'True' if given shared 'OverviewRef'" $ do
    let ref = Capability.SharedRef $ Capability.SharedReference Capability.defaultOverviewRef
    ref `shouldSatisfy` Capability.isSharedOverviewRef

  it "'isSharedOverviewRef' returns 'False' if given shared 'ArticlesRef'" $ do
    let ref = Capability.SharedRef $ Capability.SharedReference Capability.defaultArticlesRef
    ref `shouldNotSatisfy` Capability.isSharedOverviewRef

  it "'isSharedOverviewRef' returns 'False' if given shared 'ArticleRef'" $ do
    artId <- getRandomId
    let ref = Capability.SharedRef . Capability.SharedReference $ objRefWithAllArticlePerms artId
    ref `shouldNotSatisfy` Capability.isSharedOverviewRef

  it "'isSharedArticlesRef' returns 'False' if given shared 'OverviewRef'" $ do
    let ref = Capability.SharedRef $ Capability.SharedReference Capability.defaultOverviewRef
    ref `shouldNotSatisfy` Capability.isSharedArticlesRef

  it "'isSharedArticlesRef' returns 'True' if given shared 'ArticlesRef'" $ do
    let ref = Capability.SharedRef $ Capability.SharedReference Capability.defaultArticlesRef
    ref `shouldSatisfy` Capability.isSharedArticlesRef

  it "'isSharedArticlesRef' returns 'False' if given shared 'ArticleRef'" $ do
    artId <- getRandomId
    let ref = Capability.SharedRef . Capability.SharedReference $ objRefWithAllArticlePerms artId
    ref `shouldNotSatisfy` Capability.isSharedArticlesRef

  it "'isSharedArticleRef' returns 'False' if given shared 'OverviewRef'" $ do
    let ref = Capability.SharedRef $ Capability.SharedReference Capability.defaultOverviewRef
    ref `shouldNotSatisfy` Capability.isSharedArticleRef

  it "'isSharedArticleRef' returns 'False' if given shared 'ArticlesRef'" $ do
    let ref = Capability.SharedRef $ Capability.SharedReference Capability.defaultArticlesRef
    ref `shouldNotSatisfy` Capability.isSharedArticleRef

  it "'isSharedArticleRef' returns 'True' if given shared 'ArticleRef'" $ do
    artId <- getRandomId
    let ref = Capability.SharedRef . Capability.SharedReference $ objRefWithAllArticlePerms artId
    ref `shouldSatisfy` Capability.isSharedArticleRef

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

capabilityWithObjRef :: (MonadIO m) => ObjectReference -> m Capability
capabilityWithObjRef objectReference = do
  id <- getRandomId
  let petname        = Nothing
      expirationDate = Nothing
  pure $ Capability.Capability { .. }

capabilityWithNewExpDate :: (MonadIO m) => UTCTime -> m Capability
capabilityWithNewExpDate expDate = do
  id <- getRandomId
  let objectReference = Capability.defaultOverviewRef
      petname         = Nothing
      expirationDate  = Just expDate
  pure $ Capability.Capability { .. }
