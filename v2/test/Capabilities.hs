{- |
Module      : Capabilities
Description : Permission and scope inclusion tests

'capabilityTests' checks permission inclusion against membership across all 256 permission sets.
Scope tests compare representative collections and articles by the targets they contain, including repeated article IDs in different collections.
-}
module Capabilities (capabilityTests) where

import Control.Monad (forM_)
import Data.List (subsequences)
import Data.Set qualified as Set
import Eselsohr.Capability
import Eselsohr.Id (ArticleId, CollectionId, parseArticleId, parseCollectionId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

capabilityTests :: TestTree
capabilityTests = testGroup "Capability" [permissionTests, scopeTests]

------------------------------------------------------------------------
-- Permission
------------------------------------------------------------------------

permissionTests :: TestTree
permissionTests =
    testGroup
        "Permission"
        [ testCase "the vocabulary has exactly the eight approved permissions" $ do
            [minBound .. maxBound] @?= permissions
            Set.size (Set.fromList permissions) @?= 8
            length permissionSets @?= 256
        , testCase "all 65,536 pairs agree with pointwise membership" $ do
            length permissionPairs @?= 65536
            forM_ permissionPairs $ \((candidateList, candidate), (parentList, parent)) ->
                assertEqual
                    (show (candidate, parent))
                    (all (\permission -> permission `notElem` candidateList || permission `elem` parentList) permissions)
                    (permissionsSubsetOf candidate parent)
        , testCase "reflexivity" $
            forM_ permissionSets $
                \(_, value) -> assertBool (show value) (permissionsSubsetOf value value)
        , testCase "antisymmetry over every pair" $
            forM_ permissionPairs $ \((_, candidate), (_, parent)) ->
                assertEqual (show (candidate, parent)) (candidate == parent) (permissionsSubsetOf candidate parent && permissionsSubsetOf parent candidate)
        , testCase "transitivity over all 65,536 nested subset chains" $ do
            let chains =
                    [ (Set.fromList child, Set.fromList middle, Set.fromList parent)
                    | parent <- subsequences permissions
                    , middle <- subsequences parent
                    , child <- subsequences middle
                    ]

            length chains @?= 65536
            forM_ chains $ \(child, middle, parent) ->
                assertBool (show (child, middle, parent)) (permissionsSubsetOf child middle && permissionsSubsetOf middle parent && permissionsSubsetOf child parent)
        , testCase "empty and full sets bound every permission set" $
            forM_ permissionSets $ \(_, value) -> do
                permissionsSubsetOf Set.empty value @?= True
                permissionsSubsetOf value (Set.fromList permissions) @?= True
                permissionsSubsetOf value Set.empty @?= Set.null value
        , testCase "add-only inclusion neither implies Read nor requires Delegate" $ do
            let addOnly = Set.singleton AddArticles
            permissionsSubsetOf addOnly addOnly @?= True
            permissionsSubsetOf (Set.singleton ReadArticles) addOnly @?= False
            permissionsSubsetOf (Set.fromList [AddArticles, ReadArticles]) addOnly @?= False
            permissionsSubsetOf (Set.singleton Delegate) addOnly @?= False
        , testCase "giving up existing rights cannot compensate for gaining another" $ do
            let existing = Set.fromList [ReadArticles, Delegate]
                requested = Set.singleton AddArticles

            permissionsSubsetOf requested existing @?= False
            permissionsSubsetOf existing requested @?= False
        ]

permissions :: [Permission]
permissions = [ReadArticles, AddArticles, ChangeTitles, ChangeReadState, DeleteArticles, Delegate, InspectLinks, RevokeLinks]

permissionSets :: [([Permission], Permissions)]
permissionSets = [(values, Set.fromList values) | values <- subsequences permissions]

permissionPairs :: [(([Permission], Permissions), ([Permission], Permissions))]
permissionPairs = [(candidate, parent) | candidate <- permissionSets, parent <- permissionSets]

------------------------------------------------------------------------
-- Scope
------------------------------------------------------------------------

scopeTests :: TestTree
scopeTests =
    testGroup
        "Scope"
        [ testCase "all 64 pairs match target inclusion, including repeated IDs across collections" $ do
            fixtures <- scopeFixtures
            length fixtures @?= 8
            forM_ fixtures $ \(candidate, candidateTargets) ->
                forM_ fixtures $ \(parent, parentTargets) ->
                    assertEqual (show (candidate, parent)) (all (`elem` parentTargets) candidateTargets) (scopeSubsetOf candidate parent)
        , testCase "structural equality distinguishes collections, articles, and scope kinds" $ do
            fixtures <- scopeFixtures
            forM_ fixtures $ \(candidate, candidateTargets) ->
                forM_ fixtures $ \(parent, parentTargets) ->
                    assertEqual (show (candidate, parent)) (candidateTargets == parentTargets) (candidate == parent)
        , testCase "reflexivity" $ do
            fixtures <- scopeFixtures
            forM_ fixtures $ \(value, _) -> assertBool (show value) (scopeSubsetOf value value)
        , testCase "antisymmetry over every pair" $ do
            fixtures <- scopeFixtures
            forM_ fixtures $ \(candidate, _) ->
                forM_ fixtures $ \(parent, _) ->
                    assertEqual (show (candidate, parent)) (candidate == parent) (scopeSubsetOf candidate parent && scopeSubsetOf parent candidate)
        , testCase "transitivity over all 512 triples" $ do
            fixtures <- scopeFixtures
            forM_ fixtures $ \(child, _) ->
                forM_ fixtures $ \(middle, _) ->
                    forM_ fixtures $ \(parent, _) ->
                        assertBool (show (child, middle, parent)) (not (scopeSubsetOf child middle && scopeSubsetOf middle parent) || scopeSubsetOf child parent)
        ]

-- Compare scopes by the collection/article pairs they include, independently of the implementation's pattern matching.
scopeFixtures :: IO [(Scope, [(CollectionId, ArticleId)])]
scopeFixtures = do
    collections <- traverse (rightOrFail . parseCollectionId) ["123e4567-e89b-42d3-a456-426614174000", "123e4567-e89b-42d3-a456-426614174001"]
    articles <- traverse (rightOrFail . parseArticleId) ["123e4567-e89b-42d3-a456-426614174000", "123e4567-e89b-42d3-a456-426614174001", "123e4567-e89b-42d3-a456-426614174002"]

    pure . concat $
        [ (WholeCollection collection, [(collection, article) | article <- articles])
            : [(OneArticle collection article, [(collection, article)]) | article <- articles]
        | collection <- collections
        ]

rightOrFail :: (Show e) => Either e a -> IO a
rightOrFail = either (fail . show) pure
