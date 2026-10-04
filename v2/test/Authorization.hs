{- |
Module      : Authorization
Description : Operation permissions and target relationship tests

'authorizationTests' compares operation decisions with an independent permission table across every permission set and representative scopes.
Examples and generated grant trees cover child issuance, owner operations, descendant management, and expiration at the supplied decision time.
Generated trees also check that every ancestor may perform the article, collection, and management operations allowed to its descendants.
-}
module Authorization (authorizationTests) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.List (subsequences, tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (Day (..), NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime)
import Data.Tree (Tree (..))
import Eselsohr.Authorization
import Eselsohr.Capability
import Eselsohr.Id
import System.Timeout (timeout)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, QuickCheckTests (..), chooseInt, chooseInteger, conjoin, counterexample, elements, forAll, sublistOf, testProperty, (===))

authorizationTests :: TestTree
authorizationTests = localOption (QuickCheckTests 500) $ case fixtures of
    Left err -> testCase "policy fixtures" (assertFailure (show err))
    Right f -> testGroup "Authorization" [permissionTable f, examples f, structureExamples f, properties f]

------------------------------------------------------------------------
-- Permission table
------------------------------------------------------------------------

permissionTable :: Fixtures -> TestTree
permissionTable f =
    testGroup
        "Permission table"
        [ testCase "every operation agrees with the table over all 256 permission sets" $
            forM_ allRights $ \rights -> forM_ scopes $ \scope -> do
                let path = actorPath f rights scope
                forM_ (cases f path) $ \(operation, expected) ->
                    assertEqual (show (rights, scope, operation)) expected (policy now path operation == Right ())
        , testCase "removing rights never enables an operation when shared grants agree" $
            forM_ allRights $ \strong -> forM_ (map Set.fromList (subsequences (Set.toList strong))) $ \weak ->
                forM_ scopes $ \scope -> do
                    let strongPath = actorPath f strong scope
                        weakPath = actorPath f weak scope

                    forM_ (zip (cases f strongPath) (cases f weakPath)) $ \((strongOp, _), (weakOp, _)) ->
                        assertBool
                            (show (strong, weak, scope, weakOp))
                            (policy now weakPath weakOp /= Right () || policy now strongPath strongOp == Right ())
        ]
  where
    allRights = map Set.fromList (subsequences [minBound .. maxBound])
    scopes = [WholeCollection (collectionA f), OneArticle (collectionA f) (articleA f), OneArticle (collectionA f) (articleB f)]

-- The expected table uses membership and explicit target comparisons, not the production policy helpers.
cases :: Fixtures -> NonEmpty GrantSnapshot -> [(Operation, Bool)]
cases f path =
    [ (DescribeOwnGrant, True)
    , (ReadArticlesIn whole, has ReadArticles && wholeScope)
    , (ReadArticlesIn foreignWhole, False)
    , (AddArticle (collectionA f), has AddArticles && wholeScope)
    , (AddArticle (collectionB f), False)
    ]
        ++ concat
            [ [ (ReadArticlesIn (OneArticle collection article), has ReadArticles && inside collection article)
              , (RenameArticle collection article, has ReadArticles && has ChangeTitles && inside collection article)
              , (ChangeArticleReadState collection article, has ReadArticles && has ChangeReadState && inside collection article)
              , (DeleteArticle collection article, has DeleteArticles && inside collection article)
              ]
            | collection <- [collectionA f, collectionB f]
            , article <- [articleA f, articleB f]
            ]
        ++ [ (IssueChild child, has Delegate)
           , (InspectLink (grantId child) target, has InspectLinks && has Delegate)
           , (RevokeLink (grantId child) target, has RevokeLinks && has Delegate)
           , (InspectLink (grantId actor) grants, False)
           , (RevokeLink (grantId actor) grants, False)
           , (DeleteCollection (collectionA f), False)
           , (DeleteCollection (collectionB f), False)
           ]
  where
    actor = NonEmpty.head path
    grants = NonEmpty.toList path
    child = childOf (childId f) Set.empty (at 50) actor
    target = child : grants
    whole = WholeCollection (collectionA f)
    foreignWhole = WholeCollection (collectionB f)
    has permission = permission `Set.member` grantPermissions actor
    wholeScope = grantScope actor == whole
    inside collection article =
        collection == collectionA f && case grantScope actor of
            WholeCollection _ -> True
            OneArticle _ scopedArticle -> article == scopedArticle

------------------------------------------------------------------------
-- Examples
------------------------------------------------------------------------

examples :: Fixtures -> TestTree
examples f =
    testGroup
        "Examples"
        [ testCase "a grant without permissions may describe itself until it expires" $ do
            let path = actorPath f Set.empty articleScope
            policy now path DescribeOwnGrant @?= Right ()
            case checked now path of
                Left err -> assertFailure (show err)
                Right checkedActor -> decide (at 1000) checkedActor DescribeOwnGrant @?= Left (ActorChainRejected (InvalidEdge ChildExpired))
        , testCase "delete-only access deletes in scope but cannot read or edit" $ do
            let path = actorPath f (Set.singleton DeleteArticles) articleScope
            policy now path (DeleteArticle collection article) @?= Right ()
            forM_ [ReadArticlesIn articleScope, RenameArticle collection article, ChangeArticleReadState collection article, DeleteArticle collection (articleB f)] $ \operation ->
                policy now path operation @?= Left OperationDenied
        , testCase "add-only access does not read or mint Read authority" $ do
            let path = actorPath f (Set.fromList [AddArticles, Delegate]) whole
                contributor = NonEmpty.head path
            policy now path (AddArticle collection) @?= Right ()
            policy now path (ReadArticlesIn whole) @?= Left OperationDenied
            policy now path (IssueChild (childOf (childId f) (Set.singleton ReadArticles) (at 50) contributor)) @?= Left (ChildChainRejected (InvalidEdge PermissionsIncreased))
        , testCase "the owner can inspect its root and delete only its own collection" $ do
            policy now owner (InspectLink (grantId root) [root]) @?= Right ()
            policy now owner (RevokeLink (grantId root) [root]) @?= Left OperationDenied
            policy now owner (DeleteCollection collection) @?= Right ()
            policy now owner (DeleteCollection (collectionB f)) @?= Left OperationDenied
            policy now full (DeleteCollection collection) @?= Left OperationDenied
        , testCase "direct and deeper descendants are manageable by holder and owner" $
            forM_ [child :| grants, grandchild :| (child : grants)] $ \target ->
                forM_ [full, owner] $ \holderPath -> forM_ (management target) $ \operation ->
                    policy now holderPath operation @?= Right ()
        , testCase "delegated management denies self, ancestors, and siblings" $ do
            let sibling = childOf (siblingId f) Set.empty (at 50) root
            forM_ [full, owner, sibling :| [root]] $ \target ->
                forM_ (management target) $ \operation -> policy now full operation @?= Left OperationDenied
        , testCase "InspectLinks and RevokeLinks do not permit reading articles" $ do
            let path = actorPath f (Set.fromList [Delegate, InspectLinks, RevokeLinks]) whole
                target = childOf (childId f) Set.empty (at 50) (NonEmpty.head path) :| NonEmpty.toList path
            forM_ (management target) $ \operation -> policy now path operation @?= Right ()
            policy now path (ReadArticlesIn whole) @?= Left OperationDenied
        , testCase "expired targets and intermediate descendants remain manageable" $ do
            let expiredChild = child{grantExpiresAt = Just (at (-10))}
                expiredGrandchild = grandchild{grantExpiresAt = Just (at (-20))}
            forM_ [expiredChild :| grants, expiredGrandchild :| (expiredChild : grants)] $ \target ->
                forM_ (management target) $ \operation -> policy now full operation @?= Right ()
        , testCase "the supplied decision time rechecks the actor before the operation" $
            case checked now full of
                Left err -> assertFailure (show err)
                Right checkedActor -> do
                    let expiryFailure = Left (ActorChainRejected (InvalidEdge ChildExpired))
                        operation = InspectLink (grantId child) (child : grants)

                    decide (addUTCTime (-picosecond) (at 100)) checkedActor operation @?= Right ()
                    decide (at 100) checkedActor operation @?= expiryFailure
                    decide (at 101) checkedActor operation @?= expiryFailure
                    decide (at 100) checkedActor (error "operation forced") @?= expiryFailure
                    chainCheckedAt checkedActor @?= now
        , testCase "management requires matching fields for the acting grant and all shared ancestors" $ do
            let changedRoot = root{grantId = otherRootId f}
                changedActor = actor{grantParentId = Just (grantId changedRoot)}
                targets =
                    [ child :| [actor{grantExpiresAt = Just (at 99)}, root]
                    , child{grantPermissions = Set.empty} :| [actor{grantPermissions = Set.delete ChangeTitles allPermissions}, root]
                    , child :| [changedActor, changedRoot]
                    , child{grantScope = articleScope} :| [actor{grantScope = articleScope}, root]
                    , childOf (childId f) Set.empty (at 50) changedRoot :| [changedRoot]
                    ]

            forM_ targets $ \target -> do
                assertBool "target is structurally valid" (not (isLeft (structure target)))
                forM_ (management target) $ \operation -> policy now full operation @?= Left OperationDenied
        , testCase "target IDs, collections, and complete ancestry are checked" $ do
            policy now full (InspectLink (siblingId f) (child : grants)) @?= Left (TargetChainRejected RequestedGrantMismatch)
            policy now full (InspectLink (grantId child) (child{grantScope = WholeCollection (collectionB f)} : grants)) @?= Left (TargetChainRejected CollectionMismatch)
            policy now full (InspectLink (grantId child) []) @?= Left (TargetChainRejected EmptyChain)
            policy now full (RevokeLink (grantId child) [child, actor]) @?= Left (TargetChainRejected MissingRoot)
            policy now full (RevokeLink (grantId child) [child, root]) @?= Left (TargetChainRejected (InvalidEdge ParentMismatch))
        , testCase "missing management permissions deny before examining target paths" $ do
            let path = actorPath f (Set.singleton Delegate) whole
            policy now path (InspectLink (childId f) (error "target forced")) @?= Left OperationDenied
            policy now path (RevokeLink (childId f) (error "target forced")) @?= Left OperationDenied
        , testCase "issuance validates the full extended chain" $ do
            policy now full (IssueChild child) @?= Right ()
            policy now full (IssueChild child{grantId = grantId root}) @?= Left (ChildChainRejected RepeatedGrantId)
            policy now full (IssueChild child{grantParentId = Just (grantId root)}) @?= Left (ChildChainRejected (InvalidEdge ParentMismatch))
            policy now full (IssueChild child{grantExpiresAt = Just now}) @?= Left (ChildChainRejected (InvalidEdge ChildExpired))
            policy now full (IssueChild child{grantExpiresAt = Just (at 101)}) @?= Left (ChildChainRejected (InvalidEdge ChildOutlivesParent))
            policy now full (IssueChild child{grantExpiresAt = Nothing}) @?= Left (ChildChainRejected (InvalidEdge InvalidChild))

            let narrowed = actorPath f allPermissions articleScope
            policy now narrowed (IssueChild child) @?= Left (ChildChainRejected (InvalidEdge ScopeWidened))
        , testCase "depth four cannot issue another child even with Delegate" $ do
            let path = NonEmpty.reverse (leftPath (standardTree f))
                next = childOf (otherRootId f) Set.empty (at 1) (NonEmpty.head path)
            policy now path (IssueChild next) @?= Left (ChildChainRejected ChainTooLong)
        ]
  where
    collection = collectionA f
    article = articleA f
    whole = WholeCollection collection
    articleScope = OneArticle collection article
    root = rootGrant f
    owner = root :| []
    full = actorPath f allPermissions whole
    actor = NonEmpty.head full
    grants = NonEmpty.toList full
    child = childOf (childId f) allPermissions (at 50) actor
    grandchild = childOf (grandchildId f) Set.empty (at 25) child
    structure target = validateGrantStructure collection (grantId (NonEmpty.head target)) (NonEmpty.toList target)

------------------------------------------------------------------------
-- Structural validation
------------------------------------------------------------------------

structureExamples :: Fixtures -> TestTree
structureExamples f =
    testGroup
        "Structural validation"
        [ testCase "expired grants are allowed but children still need deadlines no later than their parents'" $ do
            let expiredActor = actor{grantExpiresAt = Just (at (-10))}
                expiredChild = child{grantExpiresAt = Just (at (-20))}

            check [expiredChild, expiredActor, root] @?= Right (expiredChild :| [expiredActor, root])
            check [child{grantExpiresAt = Just (at (-9))}, expiredActor, root] @?= Left (InvalidEdge ChildOutlivesParent)
            check [child{grantExpiresAt = Nothing}, expiredActor, root] @?= Left (InvalidEdge InvalidChild)
            check [child, actor, root{grantExpiresAt = Just (at (-1))}] @?= Left InvalidRoot
        , testCase "all non-time attenuation rules still apply" $ do
            check [child{grantScope = OneArticle (collectionB f) (articleA f)}, actor, root] @?= Left CollectionMismatch
            check [child{grantDepth = 4}, actor, root] @?= Left (InvalidEdge DepthMismatch)
            check [child, actor{grantPermissions = Set.delete Delegate allPermissions}, root] @?= Left (InvalidEdge DelegateRequired)
            check [child{grantPermissions = allPermissions}, actor{grantPermissions = Set.singleton Delegate}, root] @?= Left (InvalidEdge PermissionsIncreased)
            check [child, actor{grantScope = OneArticle (collectionA f) (articleA f)}, root] @?= Left (InvalidEdge ScopeWidened)
            check [child{grantParentId = Just (grantId child)}, actor, root] @?= Left (InvalidEdge InvalidChild)
            check [child, actor, actor] @?= Left RepeatedGrantId
        , testCase "structural and management traversal remain bounded before inspecting contents" $
            forM_ [repeat root, replicate 6 (error "grant evaluated") ++ error "tail forced"] $ \path -> do
                timeout 1000000 (evaluate (check path)) >>= (@?= Just (Left ChainTooLong))
                forM_ [InspectLink, RevokeLink] $ \operation ->
                    timeout 1000000 (evaluate (policy now full (operation (grantId child) path))) >>= (@?= Just (Left (TargetChainRejected ChainTooLong)))
        ]
  where
    full = actorPath f allPermissions (WholeCollection (collectionA f))
    actor = NonEmpty.head full
    root = rootGrant f
    child = childOf (childId f) Set.empty (at 50) actor
    check = validateGrantStructure (collectionA f) (grantId child)

------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------

properties :: Fixtures -> TestTree
properties f =
    testGroup
        "Properties"
        [ testProperty "every generated valid path remains structurally valid after expiry" $
            forAll (genTree f) $ \(instant, tree) ->
                conjoin
                    [ let expired = fmap (\grant -> grant{grantExpiresAt = addUTCTime (-2000000) <$> grantExpiresAt grant}) path
                          requested = grantId (NonEmpty.head path)
                          grants = NonEmpty.toList expired
                       in counterexample
                            (show expired)
                            ( validateGrantStructure (collectionA f) requested grants == Right expired
                                && (grantDepth (NonEmpty.head path) == 0 || isLeft (validateChain instant (collectionA f) requested grants))
                            )
                    | path <- treePaths tree
                    ]
        , testProperty "generated paths follow article policy and recheck expiration" $
            forAll (genTree f) $ \(instant, tree) ->
                conjoin
                    [ counterexample (show path) $
                        let actor = NonEmpty.head path
                            mayRead = ReadArticles `Set.member` grantPermissions actor
                            operation = ReadArticlesIn (grantScope actor)
                            expired = grantDepth actor > 0
                         in (policy instant path operation == Right ()) == mayRead
                                && case checked instant path of
                                    Left _ -> False
                                    Right chain -> if expired then isLeft (decide (addUTCTime 1000001 instant) chain operation) else decide (addUTCTime 1000001 instant) chain operation == Right ()
                    | path <- treePaths tree
                    ]
        , testProperty "generated management decisions agree with tree relationships, including expired targets" $
            forAll (genTree f) $ \(instant, tree) -> forAll (elements (treePaths tree)) $ \actorPath' ->
                forAll (elements (treePaths tree)) $ \targetPath ->
                    let actor = NonEmpty.head actorPath'
                        actorGrants = NonEmpty.toList actorPath'
                        targetGrants = NonEmpty.toList targetPath
                        descendant = grantId actor `elem` map grantId (NonEmpty.tail targetPath)
                        ownRoot = actorGrants == targetGrants && grantDepth actor == 0
                        mayInspect = InspectLinks `Set.member` grantPermissions actor && (descendant || ownRoot)
                        mayRevoke = RevokeLinks `Set.member` grantPermissions actor && descendant
                        expireBelow grant = if grantDepth grant > grantDepth actor then grant{grantExpiresAt = Just (addUTCTime (negate (fromIntegral (grantDepth grant))) instant)} else grant
                        expiredPath = map expireBelow targetGrants
                        requested = grantId (NonEmpty.head targetPath)
                     in conjoin
                            [ (policy instant actorPath' (operation requested target) == Right ()) === expected
                            | (operation, expected) <- [(InspectLink, mayInspect), (RevokeLink, mayRevoke)]
                            , target <- [targetGrants, expiredPath]
                            ]
        , testProperty "ancestors may perform every operation their descendants may" $
            forAll (genTree f) $ \(instant, tree) -> forAll (elements (treePaths tree)) $ \path ->
                forAll (chooseInteger (0, 1000001)) $ \offset ->
                    let decisionTime = addUTCTime (fromInteger offset) instant
                        allowed actorPath' operation = policy decisionTime actorPath' operation == Right ()
                        -- Issuance is excluded because a proposed child names its acting parent.
                        operations = fixtureOperations f ++ concatMap management (treePaths tree)
                     in conjoin
                            [ counterexample (show (ancestor, operation)) (allowed ancestor operation)
                            | ancestor <- mapMaybe NonEmpty.nonEmpty . drop 1 . tails $ NonEmpty.toList path
                            , operation <- operations
                            , allowed path operation
                            ]
        , testProperty "corrupting any generated target ancestor is rejected" $
            forAll (genTree f) $ \(instant, tree) ->
                conjoin
                    [ counterexample (show corrupted) (isLeft (policy instant (rootLabel tree :| []) (InspectLink requested corrupted)))
                    | path <- treePaths tree
                    , let grants = NonEmpty.toList path
                          requested = grantId (NonEmpty.head path)
                    , index <- [0 .. length grants - 1]
                    , corrupted <-
                        [ take index grants ++ drop (index + 1) grants
                        , changeAt index (\grant -> grant{grantDepth = maxBound}) grants
                        , changeAt index (\grant -> grant{grantParentId = Just (grantId grant)}) grants
                        , changeAt index (\grant -> grant{grantScope = WholeCollection (collectionB f)}) grants
                        ]
                    ]
        ]

------------------------------------------------------------------------
-- Policy helpers and fixtures
------------------------------------------------------------------------

checked :: UTCTime -> NonEmpty GrantSnapshot -> Either PolicyError ValidChain
checked instant path = first ActorChainRejected $ validateChain instant collection (grantId actor) (NonEmpty.toList path)
  where
    actor = NonEmpty.head path
    collection = case grantScope actor of
        WholeCollection identifier -> identifier
        OneArticle identifier _ -> identifier

policy :: UTCTime -> NonEmpty GrantSnapshot -> Operation -> Either PolicyError ()
policy instant path operation = checked instant path >>= \chain -> decide instant chain operation

-- Article and collection operations on both fixture collections and articles.
fixtureOperations :: Fixtures -> [Operation]
fixtureOperations f =
    concat
        [ [DescribeOwnGrant, ReadArticlesIn (WholeCollection collection), AddArticle collection, DeleteCollection collection]
            ++ concat
                [ [ReadArticlesIn (OneArticle collection article), RenameArticle collection article, ChangeArticleReadState collection article, DeleteArticle collection article]
                | article <- [articleA f, articleB f]
                ]
        | collection <- [collectionA f, collectionB f]
        ]

management :: NonEmpty GrantSnapshot -> [Operation]
management path = [operation (grantId (NonEmpty.head path)) (NonEmpty.toList path) | operation <- [InspectLink, RevokeLink]]

actorPath :: Fixtures -> Permissions -> Scope -> NonEmpty GrantSnapshot
actorPath f rights scope = (childOf (actorId f) rights (at 100) (rootGrant f)){grantScope = scope} :| [rootGrant f]

rootGrant :: Fixtures -> GrantSnapshot
rootGrant f = GrantSnapshot (rootId f) Nothing (WholeCollection (collectionA f)) allPermissions Nothing 0

childOf :: CapabilityId -> Permissions -> UTCTime -> GrantSnapshot -> GrantSnapshot
childOf identifier rights deadline parent = GrantSnapshot identifier (Just (grantId parent)) (grantScope parent) rights (Just deadline) (grantDepth parent + 1)

genTree :: Fixtures -> Gen (UTCTime, Tree GrantSnapshot)
genTree f = do
    instant <- at <$> chooseInteger (-1000000, 1000000)
    tree <- descend instant (rootGrant f) (subForest (identifiers f))
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
            let child = (childOf identifier rights (addUTCTime (fromInteger seconds) instant) parent){grantScope = scope}
            descend instant child grandchildren
        pure (Node parent children)

standardTree :: Fixtures -> Tree GrantSnapshot
standardTree f = build Nothing 0 (identifiers f)
  where
    build parent depth (Node identifier children) =
        let deadline = if depth == 0 then Nothing else Just (at (100 - 10 * toInteger depth))
            grant = GrantSnapshot identifier parent (WholeCollection (collectionA f)) allPermissions deadline depth
         in Node grant (map (build (Just identifier) (depth + 1)) children)

treePaths :: Tree a -> [NonEmpty a]
treePaths (Node value children) = (value :| []) : [path <> (value :| []) | child <- children, path <- treePaths child]

leftPath :: Tree a -> NonEmpty a
leftPath (Node value []) = value :| []
leftPath (Node value (child : _)) = NonEmpty.cons value (leftPath child)

changeAt :: Int -> (a -> a) -> [a] -> [a]
changeAt index change = zipWith (\position value -> if position == index then change value else value) [0 ..]

data Fixtures = Fixtures
    { collectionA :: CollectionId
    , collectionB :: CollectionId
    , articleA :: ArticleId
    , articleB :: ArticleId
    , rootId :: CapabilityId
    , actorId :: CapabilityId
    , childId :: CapabilityId
    , grandchildId :: CapabilityId
    , siblingId :: CapabilityId
    , otherRootId :: CapabilityId
    , identifiers :: Tree CapabilityId
    }

fixtures :: Either IdError Fixtures
fixtures =
    Fixtures
        <$> parseCollectionId "123e4567-e89b-42d3-a456-426614174100"
        <*> parseCollectionId "123e4567-e89b-42d3-a456-426614174101"
        <*> parseArticleId "123e4567-e89b-42d3-a456-426614174200"
        <*> parseArticleId "123e4567-e89b-42d3-a456-426614174201"
        <*> identifier 1
        <*> identifier 2
        <*> identifier 4
        <*> identifier 8
        <*> identifier 3
        <*> identifier 100
        <*> idTree 1
  where
    identifier :: Int -> Either IdError CapabilityId
    identifier number = parseCapabilityId ("123e4567-e89b-42d3-a456-" <> Text.justifyRight 12 '0' (Text.pack (show number)))
    idTree number = Node <$> identifier number <*> if number >= 16 then pure [] else traverse idTree [2 * number, 2 * number + 1]

allPermissions :: Permissions
allPermissions = Set.fromList [minBound .. maxBound]

now :: UTCTime
now = UTCTime (ModifiedJulianDay 60000) 0

at :: Integer -> UTCTime
at seconds = addUTCTime (fromInteger seconds) now

picosecond :: NominalDiffTime
picosecond = 1 / 1000000000000
