{- |
Module      : BearerTests
Description : Bearer parsing and digest lookup tests

'bearerTests' checks canonical token encoding, bearers built from their parts, and grant lookup in real SQLite transactions.
The lookup cases cover collection binding, missing and malformed grants, lock contention, and the caller's transaction lifetime.

The exported token and digest fixtures provide shared inputs for the protected article tests.
-}
module BearerTests
    ( bearerTests

      -- * Token fixtures
    , goldenToken
    , goldenDigest
    , zeroToken
    , zeroDigest
    , otherToken
    , otherDigest
    ) where

import Control.Exception (bracket_, evaluate)
import Control.Monad (forM_)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)
import Database.SQLite.Simple (Connection (..), Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Capability (GrantSnapshot (..), Scope (..))
import Eselsohr.Id (CollectionId)
import Eselsohr.Store.SQLite.Bearer
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Open (withExistingCollection, withExistingFile)
import Eselsohr.Store.SQLite.Schema (initializeSchema)
import SqliteFixtures
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, QuickCheckTests (..), arbitrary, forAll, testProperty, vectorOf)

bearerTests :: TestTree
bearerTests = testGroup "Bearers" [parserTests, lookupTests]

------------------------------------------------------------------------
-- Canonical format
------------------------------------------------------------------------

parserTests :: TestTree
parserTests =
    testGroup
        "Canonical format"
        [ testCase "golden token exposes its collection and renders unchanged" $ do
            bearer <- requireRight (parseBearer goldenToken)
            bearerCollection bearer @?= collection
            renderBearer bearer @?= goldenToken
            Text.length goldenToken @?= 69
        , testCase "zero bytes and every byte value are representable secrets" $
            forM_ [0 .. 255] $ \byte -> do
                let token = encodeToken collectionBytes (ByteString.replicate 32 byte)
                bearer <- requireRight (parseBearer token)
                renderBearer bearer @?= token
        , testCase "malformed structure, versions, lengths, and alphabets are rejected" $ do
            let collectionText = "Ej5FZ-ibQtOkVkJmFBdAAA"
                secretText = "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8"

            forM_
                ( [ ""
                  , Text.dropEnd 1 goldenToken
                  , goldenToken <> "A"
                  , "v1." <> Text.drop 3 goldenToken
                  , "V2." <> Text.drop 3 goldenToken
                  , "v3." <> Text.drop 3 goldenToken
                  , "v2.." <> secretText
                  , "v2." <> secretText <> "." <> collectionText
                  , "v2." <> Text.dropEnd 1 collectionText <> ".A" <> secretText
                  , "v2." <> collectionText <> "==." <> secretText <> "="
                  , "v2.123e4567-e89b-42d3-a456-426614174000." <> secretText
                  , "v2.%45" <> Text.drop 4 goldenToken
                  ]
                    <> [replaceCharacter 3 char goldenToken | char <- ['+', '/', '=', '.', ' ', '\t', '\n', '\0', 'é', '\xD800']]
                )
                $ assertLeft InvalidBearer . parseBearer
        , testCase "unused bits must be zero in both fields" $ do
            forM_ ['B' .. 'P'] $ \char -> assertLeft InvalidBearer (parseBearer (replaceCharacter 24 char goldenToken))
            forM_ ['9', '-', '_'] $ \char -> assertLeft InvalidBearer (parseBearer (replaceCharacter 68 char goldenToken))
        , testCase "decoded UUID version and variant use domain validation" $ do
            forM_ [ByteString.replicate 16 0, replaceByte 6 0x12 collectionBytes, replaceByte 8 0xc4 collectionBytes] $ \identifier ->
                assertLeft InvalidBearer (parseBearer (encodeToken identifier (ByteString.replicate 32 1)))
        , testCase "oversized input is rejected before decoding" $
            await (evaluate (parseBearer (Text.replicate 1000000 "A"))) >>= assertLeft InvalidBearer
        , testCase "a bearer built from its parts renders the golden token and hash" $ do
            bearer <- requireRight (newBearer collection (ByteString.pack [0 .. 31]))
            renderBearer bearer @?= goldenToken
            bearerHash bearer @?= goldenDigest
            bearerCollection bearer @?= collection
        , testCase "a built bearer needs a 32-byte secret" $
            forM_ [0, 1, 31, 33, 64] $ \size ->
                assertLeft InvalidBearer (newBearer collection (ByteString.replicate size 7))
        , localOption (QuickCheckTests 500) . testProperty "built bearers parse to the same token and hash" . forAll genParts $ \(identifier, secret) ->
            case fromCollectionBytes identifier of
                Left _ -> False
                Right owner -> case newBearer owner secret >>= \built -> (built,) <$> parseBearer (renderBearer built) of
                    Right (built, parsed) ->
                        renderBearer built == encodeToken identifier secret
                            && bearerHash parsed == bearerHash built
                            && bearerCollection parsed == owner
                    Left _ -> False
        , localOption (QuickCheckTests 500) . testProperty "generated UUIDv4 and secret bytes round trip" . forAll genParts $ \(identifier, secret) ->
            let token = encodeToken identifier secret
             in case (fromCollectionBytes identifier, parseBearer token) of
                    (Right expected, Right bearer) -> bearerCollection bearer == expected && renderBearer bearer == token
                    _ -> False
        ]

------------------------------------------------------------------------
-- Digest lookup
------------------------------------------------------------------------

lookupTests :: TestTree
lookupTests =
    testGroup
        "Digest lookup"
        [ testCase "golden digest resolves through a checked collection opening" . withEmptyDatabaseFor collection $ \_ root connection -> do
            requireRight =<< initializeSchema connection collection
            insertGrant connection collection (grantFixture collection goldenDigest)
            bearer <- requireRight (parseBearer goldenToken)
            result <- requireRight =<< withExistingCollection root collection (\opened -> inTransaction opened (lookupBearer opened bearer))
            assertSame (Right (Just (grantFixture collection goldenDigest))) result
        , testCase "independent golden hashes distinguish secrets" . withDatabase $ \connection -> do
            forM_ [(goldenToken, goldenDigest), (zeroToken, zeroDigest), (ffToken, ffDigest)] $ \(token, digest) -> do
                SQL.execute_ connection "DELETE FROM capability"
                let grant = grantFixture collection digest
                insertGrant connection collection grant
                bearer <- requireRight (parseBearer token)
                inTransaction connection (lookupBearer connection bearer) >>= assertSame (Right (Just grant))
        , testCase "unknown digest is absence and does not load permissions" . withBearerDatabase collection goldenDigest $ \_ connection -> do
            bearer <- requireRight (parseBearer zeroToken)
            SQL.execute_ connection "DROP TABLE capability_permission"
            inTransaction connection (lookupBearer connection bearer) >>= assertSame (Right Nothing)
        , testCase "an active transaction is required before reading metadata" . withEmptyDatabase $ \_ _ connection -> do
            bearer <- requireRight (parseBearer goldenToken)
            lookupBearer connection bearer >>= assertLeft TransactionRequired
            Direct.getAutoCommit (connectionHandle connection) >>= (@?= True)
        , testCase "same IDs and secrets in different collections resolve independently" $
            withBearerDatabase collection goldenDigest $ \_ first -> withBearerDatabase otherCollection otherDigest $ \_ second -> do
                firstBearer <- requireRight (parseBearer goldenToken)
                secondBearer <- requireRight (parseBearer otherToken)

                inTransaction first (lookupBearer first firstBearer) >>= assertSame (Right (Just (grantFixture collection goldenDigest)))
                inTransaction second (lookupBearer second secondBearer) >>= assertSame (Right (Just (grantFixture otherCollection otherDigest)))

                inTransaction second (lookupBearer second firstBearer) >>= assertLeft CorruptData
                inTransaction first (lookupBearer first secondBearer) >>= assertLeft CorruptData
        , testCase "copying a digest into another collection does not transfer the bearer" $
            withBearerDatabase otherCollection goldenDigest $ \_ connection -> do
                original <- requireRight (parseBearer goldenToken)
                relocated <- requireRight (parseBearer otherToken)

                inTransaction connection (lookupBearer connection original) >>= assertLeft CorruptData
                inTransaction connection (lookupBearer connection relocated) >>= assertSame (Right Nothing)
        , testCase "lookup returns an expired grant" . withBearerDatabase collection goldenDigest $ \_ connection -> do
            insertArticle connection articleFixture
            let child = childFixture{storedTokenHash = zeroDigest}
            insertGrant connection collection child
            bearer <- requireRight (parseBearer zeroToken)
            inTransaction connection (lookupBearer connection bearer) >>= assertSame (Right (Just child))
        , testCase "identity and version are checked again in the caller's transaction" . withBearerDatabase collection goldenDigest $ \_ connection -> do
            bearer <- requireRight (parseBearer goldenToken)
            inTransaction connection $ do
                lookupBearer connection bearer >>= assertSame (Right (Just (grantFixture collection goldenDigest)))

                SQL.execute_ connection "PRAGMA user_version = 2"
                lookupBearer connection bearer >>= assertLeft LookupFault

                SQL.execute connection "UPDATE collection_meta SET collection_id = ?" (Only (replaceByte 15 1 collectionBytes))
                lookupBearer connection bearer >>= assertLeft CorruptData
                assertTransaction connection
        , testCase "success preserves uncommitted changes and caller rollback" . withBearerDatabase collection goldenDigest $ \_ connection -> do
            bearer <- requireRight (parseBearer goldenToken)
            inTransaction connection $ do
                SQL.execute_ connection "UPDATE capability SET petname = 'uncommitted'"
                let changed = (grantFixture collection goldenDigest){storedPetname = Just "uncommitted"}
                lookupBearer connection bearer >>= assertSame (Right (Just changed))
                assertTransaction connection

            inTransaction connection (lookupBearer connection bearer) >>= assertSame (Right (Just (grantFixture collection goldenDigest)))
        , testCase "missing storage is a fault without ending the transaction" . withBearerDatabase collection goldenDigest $ \_ connection -> do
            bearer <- requireRight (parseBearer goldenToken)
            inTransaction connection $ do
                SQL.execute_ connection "DROP TABLE capability_permission"
                lookupBearer connection bearer >>= assertLeft LookupFault
                assertTransaction connection
        , testCase "exclusive contention fails promptly and the caller can retry" . withBearerDatabase collection goldenDigest $ \path connection -> do
            bearer <- requireRight (parseBearer goldenToken)
            SQL.withConnection path $ \holder -> bracket_ (SQL.execute_ holder "BEGIN EXCLUSIVE") (SQL.execute_ holder "ROLLBACK") $
                inTransaction connection $ do
                    await (lookupBearer connection bearer) >>= assertLeft LookupBusy
                    assertTransaction connection

            inTransaction connection (lookupBearer connection bearer) >>= assertSame (Right (Just (grantFixture collection goldenDigest)))
        , testCase "a fresh transaction observes committed deletion from another connection" . withBearerDatabase collection goldenDigest $ \path connection -> do
            bearer <- requireRight (parseBearer goldenToken)
            inTransaction connection (lookupBearer connection bearer) >>= assertSame (Right (Just (grantFixture collection goldenDigest)))
            requireRight =<< withExistingFile path (`SQL.execute_` "DELETE FROM capability")
            inTransaction connection (lookupBearer connection bearer) >>= assertSame (Right Nothing)
        , testCase "duplicate digest matches are corruption" . withLooseDatabase $ \connection -> do
            SQL.execute_ connection "INSERT INTO capability SELECT * FROM capability"
            assertCorruptLookup connection
        , testCase "invalid matched row types and grant fields are corruption" . withLooseDatabase $ \connection ->
            forM_
                [ "UPDATE capability SET id = NULL"
                , "UPDATE capability SET id = x'00'"
                , "UPDATE capability SET depth = 'private stored value'"
                , "UPDATE capability SET depth = 9223372036854775807"
                , "UPDATE capability SET expires_at = 0"
                , "UPDATE capability SET parent_id = id"
                , "UPDATE capability SET petname = CAST(x'ff' AS TEXT)"
                ]
                $ \mutation -> inTransaction connection $ do
                    SQL.execute_ connection mutation
                    bearer <- requireRight (parseBearer goldenToken)
                    lookupBearer connection bearer >>= assertLeft CorruptData
                    assertTransaction connection
        , testCase "unknown, duplicate, missing, null, and oversized permissions are corruption" . withLooseDatabase $ \connection ->
            forM_
                [ "UPDATE capability_permission SET permission = 'unknown' WHERE permission = 'read'"
                , "UPDATE capability_permission SET permission = 'read' WHERE permission = 'add'"
                , "DELETE FROM capability_permission WHERE permission = 'read'"
                , "UPDATE capability_permission SET permission = NULL WHERE permission = 'read'"
                , "UPDATE capability_permission SET capability_id = x'00' WHERE permission = 'read'"
                , "INSERT INTO capability_permission SELECT * FROM capability_permission"
                ]
                $ \mutation -> inTransaction connection $ do
                    SQL.execute_ connection mutation
                    bearer <- requireRight (parseBearer goldenToken)
                    await (lookupBearer connection bearer) >>= assertLeft CorruptData
                    assertTransaction connection
        ]

------------------------------------------------------------------------
-- Fixtures and database checks
------------------------------------------------------------------------

inTransaction :: Connection -> IO a -> IO a
inTransaction connection = bracket_ (SQL.execute_ connection "BEGIN") (SQL.execute_ connection "ROLLBACK")

assertTransaction :: Connection -> IO ()
assertTransaction connection = Direct.getAutoCommit (connectionHandle connection) >>= (@?= False)

assertCorruptLookup :: Connection -> IO ()
assertCorruptLookup connection = do
    bearer <- requireRight (parseBearer goldenToken)
    inTransaction connection (lookupBearer connection bearer) >>= assertLeft CorruptData

withBearerDatabase :: CollectionId -> ByteString -> (FilePath -> Connection -> IO a) -> IO a
withBearerDatabase owner digest use = withEmptyDatabaseFor owner $ \path _ connection -> do
    requireRight =<< initializeSchema connection owner
    insertGrant connection owner (grantFixture owner digest)
    use path connection

withLooseDatabase :: (Connection -> IO a) -> IO a
withLooseDatabase use = withBearerDatabase collection goldenDigest $ \_ connection -> do
    SQL.execute_ connection "PRAGMA foreign_keys = OFF"
    SQL.execute_ connection "DROP TABLE capability_permission"
    SQL.execute_ connection "DROP TABLE capability"
    SQL.execute_ connection "CREATE TABLE capability (id ANY, token_hash ANY, parent_id ANY, scope_article_id ANY, expires_at ANY, depth ANY, petname ANY) STRICT"
    SQL.execute_ connection "CREATE TABLE capability_permission (capability_id ANY, permission ANY) STRICT"
    SQL.execute_ connection "PRAGMA foreign_keys = ON"

    insertGrant connection collection (grantFixture collection goldenDigest)

    use connection

grantFixture :: CollectionId -> ByteString -> StoredGrant
grantFixture owner digest = rootFixture{storedGrant = (storedGrant rootFixture){grantScope = WholeCollection owner}, storedTokenHash = digest}

------------------------------------------------------------------------
-- Token fixtures
------------------------------------------------------------------------

genParts :: Gen (ByteString, ByteString)
genParts = do
    raw <- ByteString.pack <$> vectorOf 16 arbitrary
    secret <- ByteString.pack <$> vectorOf 32 arbitrary
    let identifier = replaceByte 6 ((ByteString.index raw 6 .&. 0x0f) .|. 0x40) (replaceByte 8 ((ByteString.index raw 8 .&. 0x3f) .|. 0x80) raw)
    pure (identifier, secret)

encodeToken :: ByteString -> ByteString -> Text
encodeToken identifier secret = "v2." <> encode identifier <> "." <> encode secret
  where
    encode = decodeUtf8 . Base64.encodeUnpadded

replaceCharacter :: Int -> Char -> Text -> Text
replaceCharacter index char input = Text.take index input <> Text.singleton char <> Text.drop (index + 1) input

replaceByte :: Int -> Word8 -> ByteString -> ByteString
replaceByte index byte input = ByteString.take index input <> ByteString.singleton byte <> ByteString.drop (index + 1) input

goldenToken, otherToken, zeroToken, ffToken :: Text
goldenToken = "v2.Ej5FZ-ibQtOkVkJmFBdAAA.AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8"
otherToken = "v2.Ej5FZ-ibQtOkVkJmFBdAAQ.AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8"
zeroToken = "v2.Ej5FZ-ibQtOkVkJmFBdAAA.AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
ffToken = "v2.Ej5FZ-ibQtOkVkJmFBdAAA.__________________________________________8"

-- | Independent SHA-256 fixtures over the specified prefix, UUID bytes, and secret.
goldenDigest, otherDigest, zeroDigest, ffDigest :: ByteString
goldenDigest = ByteString.pack [107, 18, 10, 209, 193, 206, 169, 58, 156, 28, 56, 130, 52, 91, 113, 24, 71, 105, 1, 75, 47, 246, 12, 29, 171, 181, 115, 70, 240, 171, 136, 81]
otherDigest = ByteString.pack [68, 17, 238, 58, 238, 223, 67, 57, 53, 76, 93, 21, 167, 221, 77, 146, 248, 60, 220, 130, 19, 170, 32, 30, 50, 4, 145, 0, 102, 197, 181, 188]
zeroDigest = ByteString.pack [221, 171, 236, 24, 2, 110, 188, 13, 113, 55, 73, 62, 88, 216, 148, 63, 237, 66, 5, 33, 25, 216, 86, 240, 157, 237, 79, 97, 141, 151, 237, 91]
ffDigest = ByteString.pack [152, 126, 107, 251, 119, 15, 135, 73, 219, 201, 150, 210, 135, 177, 27, 63, 34, 33, 14, 98, 162, 155, 214, 211, 252, 70, 28, 174, 244, 49, 34, 200]
