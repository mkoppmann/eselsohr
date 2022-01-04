module Lib.Infra.Persistence.File
  ( WithFile
  , exists
  , load
  , init
  , save
  ) where

import qualified Codec.Serialise               as Ser

import           Codec.Serialise.Class          ( Serialise )
import           Prelude                 hiding ( init )
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Directory             ( doesFileExist )
import           UnliftIO.IO.File               ( writeBinaryFileDurableAtomic )

import           Lib.App.Env                    ( DataPath
                                                , Has
                                                , grab
                                                )
import           Lib.Domain.Collection          ( Collection )
import           Lib.Domain.Id                  ( Id )
import           Lib.Infra.Persistence.Model.Collection
                                                ( CollectionPm )

type WithFile env m = (MonadReader env m, Has DataPath env, MonadUnliftIO m)

exists :: (WithFile env m) => Id Collection -> m Bool
exists colId = doesFileExist =<< idToPath colId

load :: (Serialise a, WithFile env m) => Id Collection -> (a -> b) -> m b
load colId getter = do
  filePath <- idToPath colId
  colWm    <- decodeFile filePath
  pure $ getter colWm

init :: (Serialise a, WithFile env m) => Id Collection -> a -> m ()
init colId val = flip encodeFile val =<< idToPath colId

save
  :: (WithFile env m)
  => (CollectionPm -> m CollectionPm)
  -> Id Collection
  -> m ()
save updater colId = do
  filePath <- idToPath colId
  colWm    <- decodeFile filePath
  newColWm <- updater colWm
  encodeFile filePath newColWm

decodeFile :: (Serialise a, WithFile env m) => FilePath -> m a
decodeFile fp = Ser.deserialise . fromStrict <$> liftIO (readFileBS fp)
{-# INLINE decodeFile #-}

encodeFile :: (Serialise a, WithFile env m) => FilePath -> a -> m ()
encodeFile fp = writeBinaryFileDurableAtomic fp . toStrict . Ser.serialise
{-# INLINE encodeFile #-}

idToPath :: (WithFile env m) => Id Collection -> m FilePath
idToPath colId = do
  dataPath <- grab @DataPath
  pure $ dataPath <> show colId <> ".bin"
