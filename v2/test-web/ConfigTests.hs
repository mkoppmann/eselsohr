{- |
Module      : ConfigTests
Description : Tests for parsing configuration values

The tests supply variable values through a lookup function and check required values, the listener default, listener and public origin formats, the loopback rule for plain HTTP, and the operator messages for each error.
Loading tests use temporary directories to check that the data and collections directories must exist and are never created, and compare the real environment before and after loading.
-}
module ConfigTests (configTests) where

import Data.Text qualified as Text
import Eselsohr.Config
import System.Directory (createDirectory, doesPathExist)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

configTests :: TestTree
configTests = testGroup "Configuration" [variableTests, listenerTests, originTests, loadingTests, messageTests]

variableTests :: TestTree
variableTests =
    testGroup
        "Variables"
        [ testCase "all values set" $
            fmap summary (parseConfig (values [dataDir, listen "[::1]:8080", origin "https://example.org"]))
                @?= Right ("/srv/eselsohr", Listener "::1" 8080, "https://example.org")
        , testCase "an unset listener uses the default" $
            fmap summary (parseConfig (values [dataDir, origin "http://localhost:6979"]))
                @?= Right ("/srv/eselsohr", Listener "127.0.0.1" 6979, "http://localhost:6979")
        , testCase "a missing data directory" $
            parseConfig (values [origin "https://example.org"]) @?= Left (MissingValue DataDirectoryVariable)
        , testCase "a missing public origin" $
            parseConfig (values [dataDir]) @?= Left (MissingValue PublicOriginVariable)
        , testCase "an empty data directory" $
            parseConfig (values [("ESELSOHR_DATA_DIR", ""), origin "https://example.org"])
                @?= Left (MalformedValue DataDirectoryVariable)
        , testCase "an empty listener does not fall back to the default" $
            parseConfig (values [dataDir, listen "", origin "https://example.org"])
                @?= Left (MalformedValue ListenVariable)
        , testCase "an empty public origin" $
            parseConfig (values [dataDir, origin ""]) @?= Left (MalformedValue PublicOriginVariable)
        ]
  where
    summary config = (configDataDirectory config, configListener config, renderPublicOrigin (configPublicOrigin config))
    dataDir = ("ESELSOHR_DATA_DIR", "/srv/eselsohr")
    listen value = ("ESELSOHR_LISTEN", value)
    origin value = ("ESELSOHR_PUBLIC_ORIGIN", value)

listenerTests :: TestTree
listenerTests =
    testGroup
        "Listener"
        [ testCase "accepted listeners" $
            map parseListener ["127.0.0.1:6979", "[::1]:6979", "[2001:db8::1]:80", "localhost:1", "Example.ORG:65535"]
                @?= map
                    Right
                    [ Listener "127.0.0.1" 6979
                    , Listener "::1" 6979
                    , Listener "2001:db8::1" 80
                    , Listener "localhost" 1
                    , Listener "Example.ORG" 65535
                    ]
        , testCase "rejected listeners" $
            mapM_
                (\input -> (input, parseListener input) @?= (input, Left (MalformedValue ListenVariable)))
                [ ""
                , "127.0.0.1"
                , ":6979"
                , "::1:6979"
                , "[::1]"
                , "[::1]6979"
                , "[::1:6979"
                , "[zz::1]:6979"
                , "127.0.0.1:0"
                , "127.0.0.1:65536"
                , "127.0.0.1:06979"
                , "127.0.0.1:+80"
                , "127.0.0.1: 80"
                , "999.1.1.1:80"
                , "-host:80"
                , "host_name:80"
                , "*:80"
                ]
        ]

originTests :: TestTree
originTests =
    testGroup
        "Public origin"
        [ testCase "accepted origins are kept exactly" $
            mapM_
                (\input -> fmap renderPublicOrigin (parsePublicOrigin input) @?= Right input)
                [ "https://example.org"
                , "https://example.org:8443"
                , "https://xn--bcher-kva.example"
                , "https://192.0.2.1"
                , "https://[2001:db8::1]:443"
                , "http://localhost:6979"
                , "http://127.0.0.1"
                , "http://127.255.0.1:80"
                , "http://[::1]:6979"
                ]
        , testCase "plain HTTP needs a loopback host" $
            mapM_
                (\input -> (input, parsePublicOrigin input) @?= (input, Left InsecurePublicOrigin))
                ["http://example.org", "http://10.0.0.1", "http://localhost.example.org", "http://[2001:db8::1]", "http://[::2]"]
        , testCase "malformed origins" $
            mapM_
                (\input -> (input, parsePublicOrigin input) @?= (input, Left (MalformedValue PublicOriginVariable)))
                [ "https://example.org/"
                , "https://example.org/eselsohr"
                , "https://example.org?page=1"
                , "https://example.org#top"
                , "https://user@example.org"
                , "HTTPS://example.org"
                , "https://Example.org"
                , "https://[::A]"
                , "https://bücher.example"
                , "https://example.org:0"
                , "https://example.org:65536"
                , "https://example.org:"
                , "ftp://example.org"
                , "https://"
                , "https://[::1"
                , "example.org"
                , " https://example.org"
                ]
        ]

loadingTests :: TestTree
loadingTests =
    testGroup
        "Loading"
        [ testCase "existing data and collections directories" . withTemporary $ \root -> do
            createDirectory (root </> "collections")
            result <- load root
            fmap collectionsDirectory result @?= Right (root </> "collections")
        , testCase "a missing data directory is not created" . withTemporary $ \root -> do
            result <- load (root </> "data")
            result @?= Left MissingDataDirectory
            assertAbsent (root </> "data")
        , testCase "a regular file as data directory" . withTemporary $ \root -> do
            writeFile (root </> "data") "kept"
            result <- load (root </> "data")
            result @?= Left MissingDataDirectory
            contents <- readFile (root </> "data")
            contents @?= "kept"
        , testCase "a missing collections directory is not created" . withTemporary $ \root -> do
            result <- load root
            result @?= Left MissingCollectionsDirectory
            assertAbsent (root </> "collections")
        , testCase "a regular file as collections directory" . withTemporary $ \root -> do
            writeFile (root </> "collections") ""
            result <- load root
            result @?= Left MissingCollectionsDirectory
        , testCase "a parse error stops before the directory checks" . withTemporary $ \root -> do
            result <- loadConfig (pure . values [("ESELSOHR_DATA_DIR", root </> "data"), ("ESELSOHR_PUBLIC_ORIGIN", "http://example.org")])
            result @?= Left InsecurePublicOrigin
        , testCase "loading from the process environment changes no variable" $ do
            before <- getEnvironment
            _ <- loadConfig lookupEnv
            after <- getEnvironment
            after @?= before
        ]
  where
    withTemporary = withSystemTempDirectory "eselsohr-config"
    load dataDirectory = loadConfig (pure . values [("ESELSOHR_DATA_DIR", dataDirectory), ("ESELSOHR_PUBLIC_ORIGIN", "https://example.org")])

assertAbsent :: FilePath -> Assertion
assertAbsent path = doesPathExist path >>= assertBool ("unexpected " <> path) . not

messageTests :: TestTree
messageTests =
    testCase "operator messages name the variable" $
        mapM_
            (\(err, variable) -> assertBool (show err) (variableName variable `Text.isPrefixOf` renderConfigError err))
            [ (MissingValue DataDirectoryVariable, DataDirectoryVariable)
            , (MissingValue ListenVariable, ListenVariable)
            , (MissingValue PublicOriginVariable, PublicOriginVariable)
            , (MalformedValue DataDirectoryVariable, DataDirectoryVariable)
            , (MalformedValue ListenVariable, ListenVariable)
            , (MalformedValue PublicOriginVariable, PublicOriginVariable)
            , (InsecurePublicOrigin, PublicOriginVariable)
            , (MissingDataDirectory, DataDirectoryVariable)
            , (MissingCollectionsDirectory, DataDirectoryVariable)
            ]

values :: [(String, String)] -> String -> Maybe String
values pairs name = lookup name pairs
