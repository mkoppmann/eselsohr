module Test.Domain.Id
  ( idProps
  ) where

import qualified Data.UUID.V4                                        as UUID

import           Hedgehog                                             ( (===)
                                                                      , Group(Group)
                                                                      , Property
                                                                      , property
                                                                      )

import qualified Lib.Domain.Id                                       as Id

import           Test.Domain.Shared                                   ( getRandomId )

idProps :: Group
idProps = Group "Lib.Domain.Id" [("toUuid . fromUuid ≡ True", uuidToId), ("fromUuid . toUuid ≡ True", idToUuid)]

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
