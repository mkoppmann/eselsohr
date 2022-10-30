module Test.Domain.Id
  ( idProps
  ) where

import qualified Data.UUID                                           as UUID
import qualified Data.UUID.V4                                        as UUID

import           Hedgehog                                             ( (===)
                                                                      , Group(Group)
                                                                      , Property
                                                                      , failure
                                                                      , property
                                                                      )

import qualified Lib.Domain.Id                                       as Id

import           Test.Domain.Shared                                   ( getRandomId )

idProps :: Group
idProps = Group
  "Lib.Domain.Id"
  [ ("toUuid . fromUuid ≡ True", uuidToId)
  , ("fromUuid . toUuid ≡ True", idToUuid)
  , ("toText . fromText ≡ True", textToId)
  , ("fromText . toText ≡ True", idToText)
  ]

uuidToId :: Property
uuidToId = property $ do
  randUuid <- liftIO UUID.nextRandom
  let randId = Id.fromUuid randUuid
  Id.toUuid randId === randUuid

idToUuid :: Property
idToUuid = property $ do
  randId <- getRandomId
  let randUuid = Id.toUuid randId
  Id.fromUuid randUuid === randId

textToId :: Property
textToId = property $ do
  randUuid <- UUID.toText <$> liftIO UUID.nextRandom
  case Id.fromText randUuid of
    Nothing     -> failure
    Just randId -> toText randId === randUuid

idToText :: Property
idToText = property $ do
  randId <- getRandomId
  let randText = toText randId
  case Id.fromText randText of
    Nothing         -> failure
    Just idFromText -> idFromText === randId
