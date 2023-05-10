module Test.Domain.CapabilityList
    ( capabilityListSpec
    ) where

import qualified Data.Map.Strict as Map

import Data.Map.Strict ((!))
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldNotSatisfy
    )

import qualified Lib.Domain.Authorization as Authz
import qualified Lib.Domain.Capability as Capability
import qualified Lib.Domain.CapabilityList as CapabilityList

import Lib.Domain.Article (Article)
import Lib.Domain.Capability
    ( ArticlePerms
    , Capability
    , ObjectReference
    )
import qualified Lib.Domain.Capability as Cap
import Lib.Domain.CapabilityList (CapabilityList)
import qualified Lib.Domain.CapabilityList as CapbilityList
import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Id (Id)
import Test.Domain.Shared
    ( authorized
    , getRandomId
    , objRefWithAllArticlePerms
    )

capabilityListSpec :: Spec
capabilityListSpec = describe "Lib.Domain.CapabilityList" $ do
    it "can add a new unlock link" $ do
        shouldSucceedAddingNewCapability
            Capability.defaultArticlesRef
            Authz.canCreateUnlockLinks
            CapabilityList.addUnlockLink

    it "cannot add an existing unlock link" $ do
        shouldReturnErrorAddingExistingCapability
            Capability.defaultArticlesRef
            Authz.canCreateUnlockLinks
            CapabilityList.addUnlockLink

    it "can delete an existing unlock link" $ do
        shouldSucceedDeletingExistingCapability
            Capability.defaultArticlesRef
            Authz.canDeleteUnlockLinks
            CapabilityList.removeUnlockLink

    it "can delete a non-existing unlock link" $ do
        shouldSucceedDeletingNonExistingCapability
            Capability.defaultOverviewRef
            Authz.canDeleteUnlockLinks
            CapabilityList.removeUnlockLink

    it "can add a new shared unlock link" $ do
        shouldSucceedAddingNewSharedCapability
            Capability.defaultOverviewRef
            Authz.canShareUnlockLinks
            (Capability.mkOverviewPerms True True True True)
            Capability.createSharedOverviewRef
            CapabilityList.addShareUnlockLinks

    it "cannot add an existing shared unlock link" $ do
        shouldReturnErrorAddingExistingSharedCapability
            Capability.defaultOverviewRef
            Authz.canShareUnlockLinks
            (Capability.mkOverviewPerms True True True True)
            Capability.createSharedOverviewRef
            CapabilityList.addShareUnlockLinks

    it "can delete an existing shared unlock link" $ do
        shouldSucceedDeletingExistingSharedCapability
            Capability.defaultOverviewRef
            Authz.canShareUnlockLinks
            (Capability.mkOverviewPerms True True True True)
            Capability.createSharedOverviewRef
            CapabilityList.addShareUnlockLinks
            CapbilityList.removeShareUnlockLinks

    it "can delete a non-existing shared unlock link" $ do
        shouldSucceedDeletingNonExistingCapability
            Capability.defaultOverviewRef
            Authz.canShareUnlockLinks
            CapabilityList.removeShareUnlockLinks

    it "can add a new shared article list link" $ do
        shouldSucceedAddingNewSharedCapability
            Capability.defaultArticlesRef
            Authz.canShareArticleList
            (Capability.mkArticlesPerms True True True True True True)
            Capability.createSharedArticlesRef
            CapabilityList.addShareArticleList

    it "cannot add an existing shared article list link" $ do
        shouldReturnErrorAddingExistingSharedCapability
            Capability.defaultArticlesRef
            Authz.canShareArticleList
            (Capability.mkArticlesPerms True True True True True True)
            Capability.createSharedArticlesRef
            CapabilityList.addShareArticleList

    it "can delete an existing shared article list link" $ do
        shouldSucceedDeletingExistingSharedCapability
            Capability.defaultArticlesRef
            Authz.canShareArticleList
            (Capability.mkArticlesPerms True True True True True True)
            Capability.createSharedArticlesRef
            CapabilityList.addShareArticleList
            CapbilityList.removeShareArticleList

    it "can delete a non-existing shared article list link" $ do
        shouldSucceedDeletingNonExistingCapability
            Capability.defaultArticlesRef
            Authz.canShareArticleList
            CapabilityList.removeShareArticleList

    it "can add a new shared article link" $ do
        artId <- getRandomId
        shouldSucceedAddingNewSharedCapability
            (objRefWithAllArticlePerms artId)
            (`Authz.canShareArticle` artId)
            (Capability.mkArticlePerms artId True True True True True)
            (createSharedArticleRef' artId)
            CapabilityList.addShareArticle

    it "cannot add an existing shared article link" $ do
        artId <- getRandomId
        shouldReturnErrorAddingExistingSharedCapability
            (objRefWithAllArticlePerms artId)
            (`Authz.canShareArticle` artId)
            (Capability.mkArticlePerms artId True True True True True)
            (createSharedArticleRef' artId)
            CapabilityList.addShareArticle

    it "can delete an existing shared article link" $ do
        artId <- getRandomId
        shouldSucceedDeletingExistingSharedCapability
            (objRefWithAllArticlePerms artId)
            (`Authz.canShareArticle` artId)
            (Capability.mkArticlePerms artId True True True True True)
            (createSharedArticleRef' artId)
            CapabilityList.addShareArticle
            CapbilityList.removeShareArticle

    it "can delete a non-existing shared article link" $ do
        artId <- getRandomId
        shouldSucceedDeletingNonExistingCapability
            (objRefWithAllArticlePerms artId)
            (`Authz.canShareArticle` artId)
            CapabilityList.removeShareArticle

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

shouldSucceedAddingNewCapability
    :: ObjectReference
    -> (ObjectReference -> Either AppErrorType a)
    -> (a -> Id Capability -> Capability -> CapabilityList -> Either AppErrorType CapabilityList)
    -> Expectation
shouldSucceedAddingNewCapability objRef auth update = do
    (capId, cap) <- capabilityWithObjRef objRef
    let caps = CapabilityList.mkCapabilityList
        perm = authorized $ auth objRef
        newCaps = fromRight caps $ update perm capId cap caps
        addedCap = getCapabilityWithId capId newCaps
    addedCap `shouldBe` cap

shouldReturnErrorAddingExistingCapability
    :: ObjectReference
    -> (ObjectReference -> Either AppErrorType a)
    -> (a -> Id Capability -> Capability -> CapabilityList -> Either AppErrorType CapabilityList)
    -> Expectation
shouldReturnErrorAddingExistingCapability objRef auth update = do
    (capId, cap, caps) <- capabilityListWithObjRef objRef
    let perm = authorized $ auth objRef
        err = update perm capId cap caps
    err `shouldBe` Left CapabilityList.capabilityAlreadyExists

shouldSucceedDeletingExistingCapability
    :: ObjectReference
    -> (ObjectReference -> Either AppErrorType a)
    -> (a -> Id Capability -> CapabilityList -> CapabilityList)
    -> Expectation
shouldSucceedDeletingExistingCapability objRef auth update = do
    (capId, _cap, caps) <- capabilityListWithObjRef objRef
    let perm = authorized $ auth objRef
        newCaps = update perm capId caps
    newCaps `shouldNotSatisfy` capabilityInList capId

shouldSucceedDeletingNonExistingCapability
    :: ObjectReference
    -> (ObjectReference -> Either AppErrorType a)
    -> (a -> Id Capability -> CapabilityList -> CapabilityList)
    -> Expectation
shouldSucceedDeletingNonExistingCapability objRef auth update = do
    capId <- getRandomId
    let caps = CapabilityList.mkCapabilityList
        perm = authorized $ auth objRef
        newCaps = update perm capId caps
    newCaps `shouldNotSatisfy` capabilityInList capId

shouldSucceedAddingNewSharedCapability
    :: ObjectReference
    -> (ObjectReference -> Either AppErrorType a)
    -> b
    -> (ObjectReference -> b -> Maybe ObjectReference)
    -> (a -> Id Capability -> Capability -> CapabilityList -> Either AppErrorType CapabilityList)
    -> Expectation
shouldSucceedAddingNewSharedCapability objRef auth sharedPerms sharedRef update = do
    mSharedCap <- addSharedCapability objRef auth sharedPerms sharedRef update
    case mSharedCap of
        Nothing -> expectationFailure "Could not add shared capability"
        Just (sharedCapId, sharedCap, newCaps, _perm) -> do
            let addedCap = getCapabilityWithId sharedCapId newCaps
            addedCap `shouldBe` sharedCap

shouldReturnErrorAddingExistingSharedCapability
    :: ObjectReference
    -> (ObjectReference -> Either AppErrorType a)
    -> b
    -> (ObjectReference -> b -> Maybe ObjectReference)
    -> (a -> Id Capability -> Capability -> CapabilityList -> Either AppErrorType CapabilityList)
    -> Expectation
shouldReturnErrorAddingExistingSharedCapability objRef auth sharedPerms sharedRef update = do
    mSharedCap <- addSharedCapability objRef auth sharedPerms sharedRef update
    case mSharedCap of
        Nothing -> expectationFailure "Could not add shared capability"
        Just (sharedCapId, sharedCap, newCaps, perm) -> do
            let err = update perm sharedCapId sharedCap newCaps
            err `shouldBe` Left CapabilityList.capabilityAlreadyExists

shouldSucceedDeletingExistingSharedCapability
    :: ObjectReference
    -> (ObjectReference -> Either AppErrorType a)
    -> b
    -> (ObjectReference -> b -> Maybe ObjectReference)
    -> (a -> Id Capability -> Capability -> CapabilityList -> Either AppErrorType CapabilityList)
    -> (a -> Id Capability -> CapabilityList -> CapabilityList)
    -> Expectation
shouldSucceedDeletingExistingSharedCapability objRef auth sharedPerms sharedRef update delete = do
    mSharedCap <- addSharedCapability objRef auth sharedPerms sharedRef update
    case mSharedCap of
        Nothing -> expectationFailure "Could not add shared capability"
        Just (sharedCapId, _sharedCap, newCaps, perm) -> do
            let newCaps' = delete perm sharedCapId newCaps
            newCaps' `shouldNotSatisfy` capabilityInList sharedCapId

addSharedCapability
    :: (MonadIO m)
    => ObjectReference
    -> (ObjectReference -> Either AppErrorType a)
    -> b
    -> (ObjectReference -> b -> Maybe ObjectReference)
    -> (a -> Id Capability -> Capability -> CapabilityList -> Either AppErrorType CapabilityList)
    -> m (Maybe (Id Capability, Capability, CapabilityList, a))
addSharedCapability objRef auth sharedPerms sharedRef update = do
    (_capId, _cap, caps) <- capabilityListWithObjRef objRef
    let perm = authorized $ auth objRef
    case sharedRef objRef sharedPerms of
        Nothing -> pure Nothing
        Just sharedObjRef -> do
            sharedCapId <- getRandomId
            let sharedCap = Cap.mkCapability sharedCapId sharedObjRef Nothing Nothing
                newCaps = fromRight caps $ update perm sharedCapId sharedCap caps
            pure $ Just (sharedCapId, sharedCap, newCaps, perm)

capabilityListWithObjRef :: (MonadIO m) => ObjectReference -> m (Id Capability, Capability, CapabilityList)
capabilityListWithObjRef objRef = do
    (capId, cap) <- capabilityWithObjRef objRef
    let caps = capabilityListWithCapability cap
    pure (capId, cap, caps)

capabilityWithObjRef :: (MonadIO m) => ObjectReference -> m (Id Capability, Capability)
capabilityWithObjRef objRef = do
    capId <- getRandomId
    let cap = Capability.Capability capId objRef Nothing Nothing
    pure (capId, cap)

capabilityListWithCapability :: Capability -> CapabilityList
capabilityListWithCapability cap = CapabilityList.fromMap $ Map.insert (Capability.id cap) cap Map.empty

getCapabilityWithId :: Id Capability -> CapabilityList -> Capability
getCapabilityWithId capId caps = CapabilityList.toMap caps ! capId

capabilityInList :: Id Capability -> CapabilityList -> Bool
capabilityInList capId = Map.member capId . CapabilityList.toMap

createSharedArticleRef' :: Id Article -> ObjectReference -> ArticlePerms -> Maybe ObjectReference
createSharedArticleRef' artId objRef perms = Capability.createSharedArticleRef objRef perms artId
