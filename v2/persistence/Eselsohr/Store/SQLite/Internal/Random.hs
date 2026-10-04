{- |
Module      : Eselsohr.Store.SQLite.Internal.Random
Description : Random UUIDv4 bytes from the operating system's generator

'randomUuidBytes' draws 16 bytes from the operating system's random generator through @crypton@ and sets the UUIDv4 version and variant bits, so the result decodes as a collection, grant, or article identifier.
-}
module Eselsohr.Store.SQLite.Internal.Random
    ( randomUuidBytes
    ) where

import Crypto.Random (getRandomBytes)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

randomUuidBytes :: IO ByteString
randomUuidBytes = do
    bytes <- getRandomBytes 16
    pure . ByteString.pack . zipWith mark [0 :: Int ..] $ ByteString.unpack bytes
  where
    mark 6 byte = (byte .&. 0x0f) .|. 0x40
    mark 8 byte = (byte .&. 0x3f) .|. 0x80
    mark _ byte = byte
