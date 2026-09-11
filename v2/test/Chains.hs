{-# LANGUAGE OverloadedStrings #-}

module Chains (chainTests) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (Day (..), NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime)
import Data.Tree (Tree (..))
import Eselsohr.Capability
import Eselsohr.Id
import System.Timeout (timeout)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
    ( Gen
    , QuickCheckTests (..)
    , chooseInt
    , chooseInteger
    , conjoin
    , counterexample
    , elements
    , forAll
    , sublistOf
    , testProperty
    , (===)
    )

chainTests :: TestTree
chainTests = localOption (QuickCheckTests 500) $ case fixtures of
    Left err -> testCase "chain fixtures" (assertFailure (show err))
    Right f -> testGroup "Chains" [examples f, properties f]

examples :: Fixtures -> TestTree
examples f = case NonEmpty.reverse (leftPath (standardTree f)) of
    leaf :| [third, second, first, root] ->
        let path = [leaf, third, second, first, root]
            check = validateChain now (collectionA f) (grantId leaf)
            checkPath = result now f
            expect grants failure = checkPath grants @?= Left failure
            rights = Set.fromList [ReadArticles, Delegate]
            articleScope = OneArticle (collectionA f) (articleA f)
            bounded grants = do
                outcome <- timeout 1000000 (evaluate (check grants))
                outcome @?= Just (Left ChainTooLong)
         in testGroup
                "Examples"
                [ testCase "root and every depth through four are valid" $
                    forM_ (treePaths (standardTree f)) $ \grants ->
                        case checkPath grants of
                            Left err -> assertFailure (show err)
                            Right checked -> do
                                chainGrants checked @?= grants
                                chainCheckedAt checked @?= now
                , testCase "empty input and a different requested grant are rejected" $ do
                    check [] @?= Left EmptyChain
                    validateChain now (collectionA f) (grantId root) path @?= Left RequestedGrantMismatch
                , testCase "oversized input is rejected before its contents" $ do
                    bounded (leaf : path)
                    bounded (repeat root)
                    bounded (replicate 6 (error "snapshot forced") ++ error "tail forced")
                    bounded (path ++ (error "sixth snapshot forced" : error "tail forced"))
                , testCase "collection binding checks every entry and scope kind" $
                    forM_ [0 .. 4] $ \index ->
                        forM_ [WholeCollection (collectionB f), OneArticle (collectionB f) (articleA f)] $ \scope ->
                            check (changeAt index (\grant -> grant{grantScope = scope}) path) @?= Left CollectionMismatch
                , testCase "duplicate IDs are rejected even when contents differ" $ do
                    check [leaf, third, third, first, root] @?= Left RepeatedGrantId
                    check [leaf, third, second{grantId = grantId third}, first, root] @?= Left RepeatedGrantId
                , testCase "omitted parents, reordered entries, and extra roots fail" $ do
                    check [leaf, third, second, first] @?= Left MissingRoot
                    check [leaf, third, first, root] @?= Left (InvalidEdge ParentMismatch)
                    check [leaf, third, first, second, root] @?= Left (InvalidEdge ParentMismatch)
                    expect (first{grantParentId = Nothing, grantDepth = 0, grantExpiresAt = Nothing} :| [root]) (InvalidEdge InvalidChild)
                , testCase "self and multi-node cycles fail without a repeated list entry" $ do
                    expect (leaf{grantParentId = Just (grantId leaf)} :| []) MissingRoot
                    expect (leaf :| [third{grantParentId = Just (grantId leaf)}]) MissingRoot
                    check [leaf, third, second, first{grantParentId = Just (grantId leaf)}, root] @?= Left (InvalidEdge ParentMismatch)
                    check [leaf, third, second{grantParentId = Just (grantId second)}, first, root] @?= Left (InvalidEdge InvalidChild)
                , testCase "root-only input still requires every root invariant" $ do
                    forM_ [root{grantScope = articleScope}, root{grantExpiresAt = Just (at 100)}] $ \invalid ->
                        expect (invalid :| []) InvalidRoot
                    forM_ [minBound, -1, 1, 4, maxBound] $ \depth ->
                        expect (root{grantDepth = depth} :| []) InvalidRoot
                    forM_ [minBound .. maxBound] $ \permission ->
                        expect (root{grantPermissions = Set.delete permission allPermissions} :| []) InvalidRoot
                    expect (root{grantParentId = Just (grantId first)} :| []) MissingRoot
                , testCase "leaves may lack Delegate while each parent must retain it" $ do
                    assertBool "leaf without Delegate" (not (isLeft (check [leaf{grantPermissions = Set.empty}, third, second, first, root])))
                    forM_ [1 .. 3] $ \index ->
                        check (changeAt index (\grant -> grant{grantPermissions = Set.delete Delegate allPermissions}) path) @?= Left (InvalidEdge DelegateRequired)
                , testCase "scope and permissions must remain bounded along the path" $ do
                    let narrowed = [leaf{grantScope = articleScope}, third{grantScope = articleScope}, second, first, root]
                    assertBool "narrowed path" (not (isLeft (check narrowed)))
                    check [leaf{grantScope = OneArticle (collectionA f) (articleB f)}, third{grantScope = articleScope}, second, first, root] @?= Left (InvalidEdge ScopeWidened)
                    check [leaf, third{grantScope = articleScope}, second, first, root] @?= Left (InvalidEdge ScopeWidened)
                    forM_ [0 .. 2] $ \index ->
                        check (changeAt (index + 1) (\grant -> grant{grantPermissions = rights}) path) @?= Left (InvalidEdge PermissionsIncreased)
                , testCase "invalid or skipped depths fail at every child position" $ do
                    forM_ [0 .. 3] $ \index -> do
                        forM_ [minBound, -1, 0, 5, maxBound] $ \depth ->
                            check (changeAt index (\grant -> grant{grantDepth = depth}) path) @?= Left (InvalidEdge InvalidChild)
                        check (changeAt index (\grant -> grant{grantDepth = if grantDepth grant == 1 then 2 else 1}) path) @?= Left (InvalidEdge DepthMismatch)
                , testCase "every child must have a finite future deadline" $
                    forM_ [0 .. 3] $ \index -> do
                        check (changeAt index (\grant -> grant{grantExpiresAt = Nothing}) path) @?= Left (InvalidEdge InvalidChild)
                        forM_ [addUTCTime (-picosecond) now, now] $ \deadline ->
                            check (changeAt index (\grant -> grant{grantExpiresAt = Just deadline}) path) @?= Left (InvalidEdge ChildExpired)
                , testCase "equal deadlines and the smallest future instant are valid" $ do
                    let deadline = addUTCTime picosecond now
                        sameDeadline = map (\grant -> if grantDepth grant == 0 then grant else grant{grantExpiresAt = Just deadline}) path
                    assertBool "equal future deadlines" (not (isLeft (check sameDeadline)))
                    validateChain deadline (collectionA f) (grantId leaf) sameDeadline @?= Left (InvalidEdge ChildExpired)
                    check [leaf{grantExpiresAt = Just (addUTCTime picosecond (at 70))}, third, second, first, root] @?= Left (InvalidEdge ChildOutlivesParent)
                , testCase "checked time is retained and a later validation can fail" $
                    case check path of
                        Left err -> assertFailure (show err)
                        Right checked -> do
                            chainCheckedAt checked @?= now
                            validateChain (at 60) (collectionA f) (grantId leaf) path @?= Left (InvalidEdge ChildExpired)
                            assertBool "time participates in equality" (Right checked /= validateChain (at 1) (collectionA f) (grantId leaf) path)
                            assertBool "snapshots participate in equality" (Right checked /= check [leaf{grantPermissions = Set.empty}, third, second, first, root])
                            assertBool "structural equality" (Right checked == check path)
                , testCase "structural errors precede root and edge validation" $ do
                    check [leaf{grantId = grantId root, grantScope = WholeCollection (collectionB f)}] @?= Left RequestedGrantMismatch
                    check [leaf, leaf{grantScope = WholeCollection (collectionB f)}] @?= Left CollectionMismatch
                    check [leaf, leaf] @?= Left RepeatedGrantId
                    check [leaf, root{grantParentId = Just (grantId leaf), grantDepth = 1}] @?= Left MissingRoot
                    check [leaf{grantExpiresAt = Nothing}, root{grantPermissions = Set.empty}] @?= Left InvalidRoot
                , testCase "edge failures are checked from root toward leaf" $ do
                    check [leaf{grantExpiresAt = Nothing}, third, second, first{grantExpiresAt = Just now}, root] @?= Left (InvalidEdge ChildExpired)
                    check [leaf, third, second{grantDepth = 0, grantExpiresAt = Just now}, first, root] @?= Left (InvalidEdge InvalidChild)
                ]
    _ -> testCase "five-level fixture" (assertFailure "fixture has the wrong depth")

properties :: Fixtures -> TestTree
properties f =
    testGroup
        "Properties"
        [ testProperty "every path in a generated valid tree passes unchanged" $
            forAll (genTree f) $ \(instant, tree) ->
                conjoin
                    [ case result instant f path of
                        Left err -> counterexample (show (path, err)) False
                        Right checked -> (chainGrants checked, chainCheckedAt checked) === (path, instant)
                    | path <- treePaths tree
                    ]
        , testProperty "corrupting or removing any ancestor rejects its path" $
            forAll (genTree f) $ \(instant, tree) ->
                conjoin
                    [ counterexample (show invalid) (isLeft (validateChain instant (collectionA f) (grantId (NonEmpty.head path)) invalid))
                    | path <- treePaths tree
                    , invalid <- corruptions f instant path
                    ]
        , testProperty "accepted candidates satisfy the complete ancestry laws" $
            forAll (genTree f) $ \(instant, tree) ->
                forAll (elements (treePaths tree)) $ \path ->
                    forAll (elements (NonEmpty.toList path : corruptions f instant path)) $ \candidate ->
                        case validateChain instant (collectionA f) (grantId (NonEmpty.head path)) candidate of
                            Left _ -> True
                            Right checked -> chainLaws instant f (chainGrants checked)
        ]

result :: UTCTime -> Fixtures -> NonEmpty GrantSnapshot -> Either ChainError ValidChain
result instant f path = validateChain instant (collectionA f) (grantId (NonEmpty.head path)) (NonEmpty.toList path)

chainLaws :: UTCTime -> Fixtures -> NonEmpty GrantSnapshot -> Bool
chainLaws instant f path =
    length grants <= 5
        && Set.size (Set.fromList (map grantId grants)) == length grants
        && isNothing (grantParentId root)
        && grantScope root == WholeCollection (collectionA f)
        && grantPermissions root == allPermissions
        && isNothing (grantExpiresAt root)
        && and (zipWith (\depth grant -> grantDepth grant == depth) [0 ..] (reverse grants))
        && all (\grant -> scopeSubsetOf (grantScope grant) (WholeCollection (collectionA f))) grants
        && and [grantParentId child == Just (grantId parent) && Delegate `Set.member` grantPermissions parent | (child, parent) <- zip grants (drop 1 grants)]
        && all (maybe False (instant <) . grantExpiresAt) (NonEmpty.init path)
        && and
            [ scopeSubsetOf (grantScope child) (grantScope ancestor)
                && all (`Set.member` grantPermissions ancestor) (Set.toList (grantPermissions child))
                && maybe True (\deadline -> maybe False (<= deadline) (grantExpiresAt child)) (grantExpiresAt ancestor)
            | (index, child) <- zip [0 ..] grants
            , ancestor <- drop (index + 1) grants
            ]
  where
    grants = NonEmpty.toList path
    root = NonEmpty.last path

corruptions :: Fixtures -> UTCTime -> NonEmpty GrantSnapshot -> [[GrantSnapshot]]
corruptions f instant path =
    [take index grants ++ drop (index + 1) grants | index <- indices]
        ++ [ changeAt index change grants
           | index <- indices
           , change <-
                [ \grant -> grant{grantScope = WholeCollection (collectionB f)}
                , \grant -> grant{grantDepth = maxBound}
                , \grant -> grant{grantParentId = Just (grantId grant)}
                , \grant -> grant{grantExpiresAt = Just instant}
                ]
           ]
  where
    grants = NonEmpty.toList path
    indices = [0 .. length grants - 1]

changeAt :: Int -> (a -> a) -> [a] -> [a]
changeAt index change = zipWith (\position value -> if position == index then change value else value) [0 ..]

genTree :: Fixtures -> Gen (UTCTime, Tree GrantSnapshot)
genTree f = do
    instant <- at <$> chooseInteger (-1000000, 1000000)
    let Node rootIdentifier branches = identifiers f
        root = GrantSnapshot rootIdentifier Nothing (WholeCollection (collectionA f)) allPermissions Nothing 0
    tree <- descend instant root branches
    pure (instant, tree)
  where
    descend instant parent branches = do
        count <- if Delegate `Set.member` grantPermissions parent then chooseInt (0, length branches) else pure 0
        children <- forM (take count branches) $ \(Node identifier grandchildren) -> do
            rights <- Set.fromList <$> sublistOf (Set.toList (grantPermissions parent))
            scope <- case grantScope parent of
                WholeCollection collection -> elements [WholeCollection collection, OneArticle collection (articleA f), OneArticle collection (articleB f)]
                scope@(OneArticle _ _) -> pure scope
            let upper = maybe 1000000 (floor . (`diffUTCTime` instant)) (grantExpiresAt parent)
            seconds <- chooseInteger (1, upper)
            let child = GrantSnapshot identifier (Just (grantId parent)) scope rights (Just (addUTCTime (fromInteger seconds) instant)) (grantDepth parent + 1)
            descend instant child grandchildren
        pure (Node parent children)

treePaths :: Tree a -> [NonEmpty a]
treePaths (Node value children) = (value :| []) : [path <> (value :| []) | child <- children, path <- treePaths child]

leftPath :: Tree a -> NonEmpty a
leftPath (Node value []) = value :| []
leftPath (Node value (child : _)) = NonEmpty.cons value (leftPath child)

standardTree :: Fixtures -> Tree GrantSnapshot
standardTree f = build Nothing 0 (identifiers f)
  where
    build parent depth (Node identifier children) =
        let expiry = if depth == 0 then Nothing else Just (at (100 - 10 * toInteger depth))
            grant = GrantSnapshot identifier parent (WholeCollection (collectionA f)) allPermissions expiry depth
         in Node grant (map (build (Just identifier) (depth + 1)) children)

data Fixtures = Fixtures
    { collectionA :: CollectionId
    , collectionB :: CollectionId
    , articleA :: ArticleId
    , articleB :: ArticleId
    , identifiers :: Tree CapabilityId
    }

fixtures :: Either IdError Fixtures
fixtures = do
    collection <- parseCollectionId "123e4567-e89b-42d3-a456-426614174100"
    otherCollection <- parseCollectionId "123e4567-e89b-42d3-a456-426614174101"
    firstArticle <- parseArticleId "123e4567-e89b-42d3-a456-426614174200"
    secondArticle <- parseArticleId "123e4567-e89b-42d3-a456-426614174201"
    tree <- idTree 1
    pure (Fixtures collection otherCollection firstArticle secondArticle tree)
  where
    idTree :: Int -> Either IdError (Tree CapabilityId)
    idTree number = do
        identifier <- parseCapabilityId ("123e4567-e89b-42d3-a456-" <> Text.justifyRight 12 '0' (Text.pack (show number)))
        children <- if number >= 16 then pure [] else traverse idTree [2 * number, 2 * number + 1]
        pure (Node identifier children)

allPermissions :: Permissions
allPermissions = Set.fromList [minBound .. maxBound]

now :: UTCTime
now = UTCTime (ModifiedJulianDay 60000) 0

at :: Integer -> UTCTime
at seconds = addUTCTime (fromInteger seconds) now

picosecond :: NominalDiffTime
picosecond = 1 / 1000000000000
