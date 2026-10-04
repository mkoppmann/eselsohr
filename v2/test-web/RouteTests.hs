{- |
Module      : RouteTests
Description : Tests for the route table and the fixed pages

The route tests check each entry of the table with its methods, exact path forms, and a generated property comparing 'route' with an independently written description of the table.
The page tests check that every fixed page is a complete document without scripts or styles.
-}
module RouteTests (routeTests) where

import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (isJust)
import Data.Text (Text)
import Eselsohr.Web.Html (Page (..), renderPage)
import Eselsohr.Web.Route
import Network.HTTP.Types (Method)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, checkCoverage, choose, cover, elements, forAll, frequency, oneof, testProperty, vectorOf, (===))

routeTests :: TestTree
routeTests = testGroup "Routes" [tableTests, pageTests]

tableTests :: TestTree
tableTests =
    testGroup
        "Route table"
        [ testCase "bearer page" $ do
            route "GET" ["b", "token"] @?= Just (BearerPage "token")
            route "HEAD" ["b", "token"] @?= Just (BearerPage "token")
            route "POST" ["b", "token"] @?= Nothing
        , testCase "actions" $ do
            route "POST" ["b", "token", "add"] @?= Just (BearerAction "token" AddArticle)
            route "POST" ["b", "token", "title"] @?= Just (BearerAction "token" ChangeTitle)
            route "POST" ["b", "token", "state"] @?= Just (BearerAction "token" ChangeReadState)
            route "POST" ["b", "token", "delete"] @?= Just (BearerAction "token" DeleteArticle)
        , testCase "actions accept only POST" $
            mapM_ (\method -> route method ["b", "token", "delete"] @?= Nothing) ["GET", "HEAD", "PUT", "DELETE", "PATCH", "post"]
        , testCase "deletion notice" $ do
            route "GET" ["deleted"] @?= Just DeletionNotice
            route "HEAD" ["deleted"] @?= Just DeletionNotice
            route "POST" ["deleted"] @?= Nothing
        , testCase "exact path forms" $
            mapM_
                (\segments -> route "GET" segments @?= Nothing)
                [[], [""], ["b"], ["b", "token", ""], ["b", "token", "add"], ["deleted", ""], ["B", "token"], ["b", "token", "page"]]
        , testCase "a trailing slash on an action" $
            route "POST" ["b", "token", "add", ""] @?= Nothing
        , testProperty "matches exactly the documented table" $
            checkCoverage . forAll requests $ \(method, segments) ->
                cover 5 (fst (documented method segments)) "matching" $
                    (isJust (route method segments), bearerOf (route method segments)) === documented method segments
        ]
  where
    bearerOf = \case
        Just (BearerPage bearer) -> Just bearer
        Just (BearerAction bearer _) -> Just bearer
        _ -> Nothing

-- | The route table as the design states it, written without 'route'.
documented :: Method -> [Text] -> (Bool, Maybe Text)
documented method = \case
    ["b", bearer] | method `elem` ["GET", "HEAD"] -> (True, Just bearer)
    ["b", bearer, verb] | method == "POST" && verb `elem` ["add", "title", "state", "delete"] -> (True, Just bearer)
    ["deleted"] | method `elem` ["GET", "HEAD"] -> (True, Nothing)
    _ -> (False, Nothing)

-- | Requests near the table, from shaped paths with varied segments, and random paths.
requests :: Gen (Method, [Text])
requests = (,) <$> method <*> frequency [(3, shaped), (1, noise)]
  where
    method = frequency [(3, elements ["GET", "HEAD", "POST"]), (1, elements ["PUT", "DELETE", "PATCH", "OPTIONS", "get", "post", ""])]
    shaped =
        oneof
            [ sequence [prefix, segment]
            , sequence [prefix, segment, verb]
            , sequence [elements ["deleted", "Deleted", "b"]]
            , sequence [prefix, segment, verb, segment]
            ]
    prefix = elements ["b", "b", "B", "deleted"]
    verb = elements ["add", "title", "state", "delete", "Add", "page", ""]
    noise = choose (0, 4) >>= flip vectorOf segment
    segment = elements ["b", "add", "title", "state", "delete", "deleted", "", "token", "v2.a.b", "b/"]

pageTests :: TestTree
pageTests =
    testCase "fixed pages are complete documents without scripts or styles" $
        mapM_
            ( \page -> do
                let html = LazyByteString.toStrict (renderPage page)
                assertBool (show page <> " has a doctype") (Char8.pack "<!DOCTYPE HTML>" `ByteString.isPrefixOf` html)
                assertBool (show page <> " declares UTF-8") (contains "<meta charset=\"utf-8\">" html)
                assertBool (show page <> " has no script") (not (contains "<script" html))
                assertBool (show page <> " has no style") (not (contains "<style" html || contains "style=" html))
            )
            [minBound .. maxBound :: Page]
  where
    contains needle = ByteString.isInfixOf (Char8.pack needle)
