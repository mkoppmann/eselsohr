module Lib.Impl.Repository.File
  ( WithFile,
    load,
    save,
    exists,
    init,

    -- * Error helpers
    extractionError,
  )
where

import Codec.Compression.Zstd (Decompress (..), compress, decompress)
import qualified Codec.Serialise as Ser
import Codec.Serialise.Class (Serialise)
import qualified Data.Sequence as Seq
import Lib.App.Env (DataPath, Has, MaxConcurrentWrites, WriteQueue, grab)
import Lib.App.Error (AppErrorType, WithError, storeError, throwError, throwOnNothing)
import Lib.Core.Domain (Id)
import Lib.Core.Domain.StoreEvent (StoreEvent, apply)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)
import Prelude hiding (init)

zsdtCompressionLvl :: Int
zsdtCompressionLvl = 1

type WithFile env m =
  ( MonadReader env m,
    Has DataPath env,
    Has WriteQueue env,
    Has (Maybe MaxConcurrentWrites) env,
    MonadUnliftIO m
  )

load :: (Serialise a, WithError m, WithFile env m) => Id a -> (a -> b) -> m b
load resId getter = getter <$> (decodeFile =<< idToPath resId)

save :: (WithError m, WithFile env m) => Id a -> Seq StoreEvent -> m ()
save resId updates = do
  fp <- idToPath resId
  content <- decodeFile fp
  -- We need to reverse the order of updates because of how foldr traverses the
  -- list.
  let newContent = foldr apply content $ Seq.reverse updates
  encodeFile fp newContent

exists :: (WithFile env m) => Id a -> m Bool
exists resId = doesFileExist =<< idToPath resId

init :: (Serialise a, WithFile env m) => Id a -> a -> m ()
init resId val = flip encodeFile val =<< idToPath resId

encodeFile :: (Serialise a, WithFile env m) => FilePath -> a -> m ()
encodeFile fp =
  writeBinaryFileDurableAtomic fp . compress zsdtCompressionLvl . toStrict . Ser.serialise
{-# INLINE encodeFile #-}

decodeFile :: (Serialise a, WithError m, WithFile env m) => FilePath -> m a
decodeFile fp =
  Ser.deserialise . fromStrict <$> (handleDecompress =<< liftIO (readFileBS fp))
  where
    handleDecompress :: (WithError m) => ByteString -> m ByteString
    handleDecompress bs = case decompress bs of
      Decompress newBs -> pure newBs
      Error message -> decompressionException $ toText message
      Skip -> decompressionException "The compressed frame was empty."
{-# INLINE decodeFile #-}

idToPath :: (WithFile env m) => Id a -> m FilePath
idToPath resId = do
  dataPath <- grab @DataPath
  let showId = show resId
  let fileEnding = ".bin"
  pure $ dataPath <> showId <> fileEnding

-- * Error helpers

decompressionException :: (WithError m) => Text -> m a
decompressionException = withFrozenCallStack . throwError . decompressionError

decompressionError :: Text -> AppErrorType
decompressionError reason =
  storeError $ "Could not decompress loaded resource, because: " <> reason

extractionError :: (WithError m) => Maybe a -> m a
extractionError = withFrozenCallStack . throwOnNothing singleEntryError

singleEntryError :: AppErrorType
singleEntryError = storeError "Could not extract resource from file"
