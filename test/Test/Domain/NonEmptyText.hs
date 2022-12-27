module Test.Domain.NonEmptyText
    ( nonEmptyTextProps
    ) where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog
    ( Group (Group)
    , Property
    , assert
    , evalEither
    , forAll
    , property
    )

import qualified Lib.Domain.NonEmptyText as NET

nonEmptyTextProps :: Group
nonEmptyTextProps =
    Group
        "Lib.Domain.NonEmptyText"
        [ ("it does not accept empty text", doesNotAcceptEmptyText)
        , ("it can wrap any non-empty text", canWrapAnyNonEmptyText)
        ]

doesNotAcceptEmptyText :: Property
doesNotAcceptEmptyText = property $ do
    emptyText <- forAll $ Gen.text (Range.singleton 0) Gen.unicodeAll
    assert . isLeft $ NET.fromText emptyText "Property error"

canWrapAnyNonEmptyText :: Property
canWrapAnyNonEmptyText = property $ do
    nonEmptyText <- forAll $ Gen.text (Range.linear 1 100) Gen.unicodeAll
    void . evalEither $ NET.fromText nonEmptyText "Property error"
