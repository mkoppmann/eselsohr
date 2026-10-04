{- |
Module      : StoreInterface
Description : External consumer checks of the application-facing Store interface

This suite depends on the public library only, not on the private SQLite or runtime libraries, so it uses "Eselsohr.Store" as application code would.
'storeInterfaceTests' checks that revisions stay outside article values and that failure values render without data.
'_titlesOf' is a read-only application function whose type admits no write operation and no IO, and '_mayAdd' uses only the grant description; both only need to compile.

The absence of connections, opening functions, SQL, data paths, clocks, and binding constructors rests on this suite's dependencies and a review of the exports; no negative compilation test runs.
-}
module StoreInterface (storeInterfaceTests) where

import Data.Set qualified as Set
import Data.Time (UTCTime (..), fromGregorian)
import Effectful (Eff, (:>))
import Eselsohr.Article (Article (..), Title, mkTitle, newArticle)
import Eselsohr.Capability (Permission (..), Scope (..))
import Eselsohr.Id (parseArticleId)
import Eselsohr.Store
import Eselsohr.WebUrl (parseWebUrl)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

storeInterfaceTests :: TestTree
storeInterfaceTests =
    testGroup
        "Store interface"
        [ testCase "revisions stay outside article values" $ do
            article <- sampleArticle
            let stored = Revised article (Revision 3)
            assertBool "article value changed" (revisedValue stored == article)
            revision stored @?= Revision 3
        , testCase "failure values render without data" $ do
            map show [ReadDenied, ReadFault Busy, ReadFault Corrupt, ReadFault Unavailable]
                @?= ["ReadDenied", "ReadFault Busy", "ReadFault Corrupt", "ReadFault Unavailable"]
            map show [EditDenied, EditNotFound, EditConflict, EditInvalidRevision, EditRevisionExhausted, EditFault Busy]
                @?= ["EditDenied", "EditNotFound", "EditConflict", "EditInvalidRevision", "EditRevisionExhausted", "EditFault Busy"]
        ]

-- | Whether the bound grant may add articles to its whole collection, using only the grant description.
_mayAdd :: (GrantReader :> es) => Eff es (Either ReadFailure Bool)
_mayAdd = fmap allowed <$> describeGrant
  where
    allowed view = AddArticles `Set.member` viewPermissions view && isWhole (viewScope view)
    isWhole = \case
        WholeCollection _ -> True
        OneArticle _ _ -> False

-- | Titles of the bound collection, using only the read effect.
_titlesOf :: (ArticleReader :> es) => Eff es (Either ReadFailure [Title])
_titlesOf = fmap (map (articleTitle . revisedValue)) <$> listArticles AllArticles

sampleArticle :: IO Article
sampleArticle = either assertFailure pure $ do
    identifier <- describe (parseArticleId "123e4567-e89b-42d3-a456-426614174002")
    title <- describe (mkTitle "A title")
    url <- describe (parseWebUrl "https://example.com/")
    pure (newArticle identifier title url (UTCTime (fromGregorian 2026 10 4) 0))
  where
    describe :: (Show e) => Either e a -> Either String a
    describe = either (Left . show) Right
