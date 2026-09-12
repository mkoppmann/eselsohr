{- |
Module      : Attenuation
Description : Delegation checks for parent and child grants

'attenuationTests' exercises the local delegation rules for permissions, scope, expiration, parent identity, and depth.
Examples check failures and their precedence; generated grant pairs check acceptance and preservation of validity under further restriction.
-}
module Attenuation (attenuationTests) where

import Control.Monad (forM_)
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Time (Day (..), NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime)
import Eselsohr.Capability
import Eselsohr.Id
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
    ( Gen
    , QuickCheckTests (..)
    , chooseInt
    , chooseInteger
    , counterexample
    , elements
    , forAll
    , sublistOf
    , testProperty
    , (===)
    )

attenuationTests :: TestTree
attenuationTests = localOption (QuickCheckTests 500) $ case fixtures of
    Left err -> testCase "attenuation fixtures" (assertFailure (show err))
    Right f -> testGroup "Attenuation" [examples f, properties f]

------------------------------------------------------------------------
-- Examples
------------------------------------------------------------------------

examples :: Fixtures -> TestTree
examples f =
    testGroup
        "Examples"
        [ testCase "root and intermediate parents may delegate" $ do
            maxDelegationDepth @?= 4
            attenuates now root parent @?= Right ()
            attenuates now parent child @?= Right ()
        , testCase "a root requires whole collection, no expiry, and depth zero" $
            forM_ [root{grantScope = articleScope}, root{grantExpiresAt = Just (at 100)}, root{grantParentId = Just (grantId parent)}] $
                \invalid -> attenuates now invalid parent @?= Left InvalidParent
        , testCase "every root permission is required" $
            forM_ [minBound .. maxBound] $ \permission ->
                attenuates now root{grantPermissions = Set.delete permission allPermissions} parent @?= Left InvalidParent
        , testCase "malformed delegated parents are rejected" $
            forM_ [parent{grantParentId = Nothing}, parent{grantParentId = Just (grantId parent)}, parent{grantExpiresAt = Nothing}] $
                \invalid -> attenuates now invalid child @?= Left InvalidParent
        , testCase "malformed children and roots supplied as children are rejected" $
            forM_ [child{grantParentId = Nothing}, child{grantParentId = Just (grantId child)}, child{grantExpiresAt = Nothing}, root] $
                \invalid -> attenuates now parent invalid @?= Left InvalidChild
        , testCase "invalid depths cannot wrap or create a valid edge" $ do
            forM_ [minBound, -1, 1, 4, 5, maxBound] $ \depth ->
                attenuates now root{grantDepth = depth} parent @?= Left InvalidParent
            forM_ [minBound, -1, 0, 5, maxBound] $ \depth -> do
                attenuates now parent{grantDepth = depth} child @?= Left InvalidParent
                attenuates now parent child{grantDepth = depth} @?= Left InvalidChild
        , testCase "parent identity is bound and IDs cannot be reused" $ do
            attenuates now parent child{grantParentId = Just (grantId root)} @?= Left ParentMismatch
            attenuates now parent child{grantId = grantId parent, grantParentId = Just (grantId root)} @?= Left ParentMismatch
        , testCase "depth must advance exactly once" $
            forM_ [1, 3, 4] $
                \depth -> attenuates now parent child{grantDepth = depth} @?= Left DepthMismatch
        , testCase "a depth-four child may have Delegate but cannot delegate further" $ do
            attenuates now parent{grantDepth = 3} child{grantDepth = 4, grantPermissions = Set.singleton Delegate} @?= Right ()
            attenuates now parent{grantDepth = 4} child @?= Left DepthExhausted
        , testCase "Delegate is required without implying other permissions" $ do
            attenuates now parent{grantPermissions = Set.delete Delegate (grantPermissions parent)} child @?= Left DelegateRequired
            attenuates now parent{grantPermissions = Set.fromList [AddArticles, Delegate]} child{grantPermissions = Set.singleton AddArticles} @?= Right ()
            attenuates now parent child{grantPermissions = Set.insert ChangeTitles (grantPermissions child)} @?= Left PermissionsIncreased
        , testCase "scope may narrow but cannot cross articles or collections" $ do
            attenuates now parent child{grantScope = articleScope} @?= Right ()
            attenuates now parent{grantScope = articleScope} child{grantScope = articleScope} @?= Right ()
            forM_ [WholeCollection (collectionA f), OneArticle (collectionA f) (articleB f), OneArticle (collectionB f) (articleA f)] $ \scope ->
                attenuates now parent{grantScope = articleScope} child{grantScope = scope} @?= Left ScopeWidened
            attenuates now parent child{grantScope = WholeCollection (collectionB f)} @?= Left ScopeWidened
            attenuates now parent{grantScope = articleScope} child{grantPermissions = Set.empty} @?= Left ScopeWidened
        , testCase "full permissions do not make a delegated child a root" $ do
            let fullChild = parent{grantPermissions = allPermissions}
            attenuates now root fullChild @?= Right ()
            attenuates now root fullChild{grantParentId = Nothing, grantDepth = 0, grantExpiresAt = Nothing} @?= Left InvalidChild
        , testCase "parent and child expiry use a strict deadline" $ do
            forM_ [addUTCTime (-picosecond) now, now] $ \deadline -> do
                attenuates now parent{grantExpiresAt = Just deadline} child @?= Left ParentExpired
                attenuates now parent child{grantExpiresAt = Just deadline} @?= Left ChildExpired

            let future = Just (addUTCTime picosecond now)
            attenuates now parent{grantExpiresAt = future} child{grantExpiresAt = future} @?= Right ()
            attenuates now parent child{grantExpiresAt = future} @?= Right ()
        , testCase "child may share a parent deadline but cannot outlive it" $ do
            attenuates now parent child{grantExpiresAt = Just (at 100)} @?= Right ()
            attenuates now parent child{grantExpiresAt = Just (addUTCTime picosecond (at 100))} @?= Left ChildOutlivesParent
        , testCase "seven days is not an attenuation lifetime cap" $
            attenuates now root parent{grantExpiresAt = Just (at (30 * 86400))} @?= Right ()
        , testCase "grant equality compares every field" $ do
            let variants =
                    [ child{grantId = grantId root}
                    , child{grantParentId = Just (grantId root)}
                    , child{grantScope = articleScope}
                    , child{grantPermissions = Set.empty}
                    , child{grantExpiresAt = Just (at 51)}
                    , child{grantDepth = 3}
                    ]
                rebuilt = GrantSnapshot (grantId child) (grantParentId child) (grantScope child) (grantPermissions child) (grantExpiresAt child) (grantDepth child)

            rebuilt @?= child
            forM_ variants $ \different -> assertBool (show different) (child /= different)
        , testCase "overlapping faults preserve the documented error order" $
            forM_ precedence $
                \(expected, p, c) -> assertEqual (show expected) (Left expected) (attenuates now p c)
        ]
  where
    root = rootGrant f
    parent = parentGrant f
    child = childGrant f
    articleScope = OneArticle (collectionA f) (articleA f)
    foreignScope = WholeCollection (collectionB f)
    extraRights = Set.insert ChangeTitles (grantPermissions child)
    precedence =
        [ (InvalidParent, parent{grantDepth = 0, grantExpiresAt = Just now}, child)
        , (ParentExpired, parent{grantExpiresAt = Just now, grantPermissions = Set.empty}, child)
        , (DelegateRequired, parent{grantDepth = 4, grantPermissions = Set.empty}, child)
        , (DepthExhausted, parent{grantDepth = 4}, child{grantExpiresAt = Nothing})
        , (InvalidChild, parent, child{grantDepth = 0, grantExpiresAt = Just now})
        , (ChildExpired, parent, child{grantExpiresAt = Just now, grantParentId = Just (grantId root)})
        , (ParentMismatch, parent, child{grantParentId = Just (grantId root), grantDepth = 3})
        , (DepthMismatch, parent, child{grantDepth = 3, grantScope = foreignScope})
        , (ScopeWidened, parent, child{grantScope = foreignScope, grantPermissions = extraRights})
        , (PermissionsIncreased, parent, child{grantPermissions = extraRights, grantExpiresAt = Just (at 101)})
        ]

------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------

properties :: Fixtures -> TestTree
properties f =
    testGroup
        "Properties"
        [ testProperty "generated valid edges pass" . forAll (genValidEdge f) $ \(instant, parent, child) ->
            attenuates instant parent child === Right ()
        , testProperty "successful edges satisfy every local invariant" . forAll (genCandidateEdge f) $ \(instant, parent, child) ->
            attenuates instant parent child /= Right () || edgeLaws instant parent child
        , testProperty "further restriction preserves acceptance" . forAll (genValidEdge f) $ \(instant, parent, child) ->
            case grantExpiresAt child of
                Nothing -> counterexample "generated child has no deadline" False
                Just deadline -> forAll (genRestrictedChild f instant deadline child) $ \restricted ->
                    attenuates instant parent restricted === Right ()
        ]

------------------------------------------------------------------------
-- Grant generators and fixtures
------------------------------------------------------------------------

genValidEdge :: Fixtures -> Gen (UTCTime, GrantSnapshot, GrantSnapshot)
genValidEdge f = do
    instant <- at <$> chooseInteger (-1000000, 1000000)
    depth <- chooseInt (0, 3)
    parentSeconds <- chooseInteger (1, 1000000)
    childSeconds <- chooseInteger (1, parentSeconds)
    rights <- Set.insert Delegate . Set.fromList <$> sublistOf [minBound .. maxBound]
    scope <- genNarrowScope f (grantScope (rootGrant f))
    let parent =
            if depth == 0
                then rootGrant f
                else (parentGrant f){grantDepth = depth, grantPermissions = rights, grantScope = scope, grantExpiresAt = Just (addUTCTime (fromInteger parentSeconds) instant)}

    childRights <- Set.fromList <$> sublistOf (Set.toList (grantPermissions parent))
    childScope <- genNarrowScope f (grantScope parent)
    let child = (childGrant f){grantParentId = Just (grantId parent), grantDepth = depth + 1, grantPermissions = childRights, grantScope = childScope, grantExpiresAt = Just (addUTCTime (fromInteger childSeconds) instant)}
    pure (instant, parent, child)

genNarrowScope :: Fixtures -> Scope -> Gen Scope
genNarrowScope f scope = case scope of
    WholeCollection collection -> elements [scope, OneArticle collection (articleA f), OneArticle collection (articleB f)]
    OneArticle _ _ -> pure scope

genRestrictedChild :: Fixtures -> UTCTime -> UTCTime -> GrantSnapshot -> Gen GrantSnapshot
genRestrictedChild f instant deadline child = do
    rights <- Set.fromList <$> sublistOf (Set.toList (grantPermissions child))
    scope <- genNarrowScope f (grantScope child)
    seconds <- chooseInteger (1, floor (diffUTCTime deadline instant))
    pure child{grantPermissions = rights, grantScope = scope, grantExpiresAt = Just (addUTCTime (fromInteger seconds) instant)}

genCandidateEdge :: Fixtures -> Gen (UTCTime, GrantSnapshot, GrantSnapshot)
genCandidateEdge f = do
    (instant, parent, child) <- genValidEdge f
    depth <- elements [minBound, -1, 0, 1, 2, 3, 4, 5, maxBound]
    (p, c) <-
        elements
            [ (parent, child)
            , (parent{grantDepth = depth}, child)
            , (parent, child{grantDepth = depth})
            , (parent{grantPermissions = Set.empty}, child)
            , (parent, child{grantPermissions = allPermissions})
            , (parent{grantExpiresAt = Just instant}, child)
            , (parent, child{grantExpiresAt = Just instant})
            , (parent, child{grantExpiresAt = Nothing})
            , (parent, child{grantParentId = Nothing})
            , (parent, child{grantParentId = Just (grantId child)})
            , (parent, child{grantScope = WholeCollection (collectionB f)})
            , (parent, rootGrant f)
            ]
    pure (instant, p, c)

edgeLaws :: UTCTime -> GrantSnapshot -> GrantSnapshot -> Bool
edgeLaws instant parent child =
    parentShape
        && Delegate `Set.member` grantPermissions parent
        && grantId parent /= grantId child
        && grantParentId child == Just (grantId parent)
        && grantDepth parent >= 0
        && grantDepth parent < 4
        && grantDepth child == grantDepth parent + 1
        && scopeSubsetOf (grantScope child) (grantScope parent)
        && all (`Set.member` grantPermissions parent) (Set.toList (grantPermissions child))
        && case grantExpiresAt child of
            Nothing -> False
            Just deadline -> instant < deadline && maybe True (deadline <=) (grantExpiresAt parent)
  where
    parentShape = case grantParentId parent of
        Nothing -> grantDepth parent == 0 && isNothing (grantExpiresAt parent) && grantPermissions parent == allPermissions && wholeCollection (grantScope parent)
        Just identifier -> identifier /= grantId parent && grantDepth parent >= 1 && maybe False (instant <) (grantExpiresAt parent)
    wholeCollection (WholeCollection _) = True
    wholeCollection (OneArticle _ _) = False

data Fixtures = Fixtures
    { rootGrant :: GrantSnapshot
    , parentGrant :: GrantSnapshot
    , childGrant :: GrantSnapshot
    , collectionA :: CollectionId
    , collectionB :: CollectionId
    , articleA :: ArticleId
    , articleB :: ArticleId
    }

fixtures :: Either IdError Fixtures
fixtures = do
    rootId <- parseCapabilityId "123e4567-e89b-42d3-a456-426614174000"
    parentId <- parseCapabilityId "123e4567-e89b-42d3-a456-426614174001"
    childId <- parseCapabilityId "123e4567-e89b-42d3-a456-426614174002"
    collection <- parseCollectionId "123e4567-e89b-42d3-a456-426614174100"
    otherCollection <- parseCollectionId "123e4567-e89b-42d3-a456-426614174101"
    firstArticle <- parseArticleId "123e4567-e89b-42d3-a456-426614174200"
    secondArticle <- parseArticleId "123e4567-e89b-42d3-a456-426614174201"

    let scope = WholeCollection collection
        root = GrantSnapshot rootId Nothing scope allPermissions Nothing 0
        parent = GrantSnapshot parentId (Just rootId) scope (Set.fromList [ReadArticles, AddArticles, Delegate]) (Just (at 100)) 1
        child = GrantSnapshot childId (Just parentId) scope (Set.singleton ReadArticles) (Just (at 50)) 2
    pure (Fixtures root parent child collection otherCollection firstArticle secondArticle)

allPermissions :: Permissions
allPermissions = Set.fromList [minBound .. maxBound]

now :: UTCTime
now = UTCTime (ModifiedJulianDay 60000) 0

at :: Integer -> UTCTime
at seconds = addUTCTime (fromInteger seconds) now

picosecond :: NominalDiffTime
picosecond = 1 / 1000000000000
