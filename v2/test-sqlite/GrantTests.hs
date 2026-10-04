{- |
Module      : GrantTests
Description : Protected grant description tests

'grantTests' checks that the description returns exactly the permissions, scope, and expiration of the presented grant, for the owner, a delegated one-article reader with a petname, and an add-only contributor.
Unknown, expired, and revoked bearers are denied, and the clock is sampled once inside the read transaction.
-}
module GrantTests (grantTests) where

import BearerTests (goldenDigest, goldenToken, zeroDigest, zeroToken)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection (..), Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Capability
import Eselsohr.Store (GrantView (..))
import Eselsohr.Store.SQLite.Articles (StoreError (..))
import Eselsohr.Store.SQLite.Bearer (Bearer, newBearer, parseBearer)
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Grants (describeGrant)
import Eselsohr.Store.SQLite.Schema (initializeSchema)
import SqliteFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

grantTests :: TestTree
grantTests =
    testGroup
        "Grant description"
        [ testCase "the owner sees every permission on the whole collection without expiration" . withGrantDatabase $ \connection -> do
            owner <- bearerFor goldenToken
            describeGrant connection owner (pure instant)
                >>= (@?= Right (GrantView (Set.fromList [minBound .. maxBound]) (WholeCollection collection) Nothing))
        , testCase "a delegated reader sees its permissions, article, and deadline, but not its petname" . withGrantDatabase $ \connection -> do
            insertGrant connection collection reader
            child <- bearerFor zeroToken
            describeGrant connection child (pure instant)
                >>= (@?= Right (GrantView (Set.singleton ReadArticles) (OneArticle collection articleIdentifier) (Just deadline)))
        , testCase "an add-only contributor sees only its own grant" . withGrantDatabase $ \connection -> do
            insertGrant connection collection contributor
            child <- bearerFor zeroToken
            describeGrant connection child (pure instant)
                >>= (@?= Right (GrantView (Set.singleton AddArticles) (WholeCollection collection) (Just deadline)))
        , testCase "unknown, expired, and revoked bearers are denied" . withGrantDatabase $ \connection -> do
            insertGrant connection collection reader
            child <- bearerFor zeroToken
            unknown <- requireRight (newBearer collection (ByteString.replicate 32 0xee))

            describeGrant connection unknown (pure instant) >>= (@?= Left StoreDenied)
            describeGrant connection child (pure deadline) >>= (@?= Left StoreDenied)
            SQL.execute connection "DELETE FROM capability WHERE token_hash = ?" (Only zeroDigest)
            describeGrant connection child (pure instant) >>= (@?= Left StoreDenied)
        , testCase "the clock is sampled once inside the read transaction" . withGrantDatabase $ \connection -> do
            owner <- bearerFor goldenToken
            samples <- newIORef []
            let clock = do
                    autocommit <- Direct.getAutoCommit (connectionHandle connection)
                    modifyIORef' samples (autocommit :)
                    pure instant
            _ <- describeGrant connection owner clock
            readIORef samples >>= (@?= [False])
            Direct.getAutoCommit (connectionHandle connection) >>= (@?= True)
        ]
  where
    reader = childFixture{storedGrant = (storedGrant childFixture){grantExpiresAt = Just deadline}, storedTokenHash = zeroDigest}
    contributor =
        StoredGrant
            { storedGrant = GrantSnapshot childIdentifier (Just rootIdentifier) (WholeCollection collection) (Set.singleton AddArticles) (Just deadline) 1
            , storedTokenHash = zeroDigest
            , storedPetname = Just "contributor"
            }

instant, deadline :: UTCTime
instant = fromTimestamp 1000000
deadline = fromTimestamp 2000000

withGrantDatabase :: (Connection -> IO a) -> IO a
withGrantDatabase use = withEmptyDatabase $ \_ _ connection -> do
    requireRight =<< initializeSchema connection collection
    insertGrant connection collection rootFixture{storedTokenHash = goldenDigest}
    insertArticle connection articleFixture
    use connection

bearerFor :: Text -> IO Bearer
bearerFor = requireRight . parseBearer
