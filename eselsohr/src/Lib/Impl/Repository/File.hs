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

import Data.Binary (Binary)
import qualified Data.Binary as Bin
import Data.Binary.Get (ByteOffset, Decoder (..), runGetIncremental)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.Sequence as Seq
import Lib.App.Env (DataPath, Has, MaxConcurrentWrites, WriteQueue, grab)
import Lib.App.Error (AppErrorType, WithError, storeError, throwError, throwOnNothing)
import Lib.Core.Domain (Id)
import Lib.Core.Domain.StoreEvent (StoreEvent, apply)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.IO.File (withBinaryFileDurableAtomic, writeBinaryFileDurableAtomic)
import Prelude hiding (init)

type WithFile env m =
  ( MonadReader env m,
    Has DataPath env,
    Has WriteQueue env,
    Has (Maybe MaxConcurrentWrites) env,
    MonadUnliftIO m
  )

load :: (Binary a, WithError m, WithFile env m) => Id a -> (a -> Maybe b) -> m b
load resId getter = extractionError . getter =<< decodeFile =<< idToPath resId

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

init :: (Binary a, WithFile env m) => Id a -> a -> m ()
init resId val = flip encodeFile val =<< idToPath resId

encodeFile :: (Binary a, WithFile env m) => FilePath -> a -> m ()
encodeFile fp = writeBinaryFileDurableAtomic fp . L.toStrict . Bin.encode
{-# INLINE encodeFile #-}

decodeFile :: (Binary a, WithError m, WithFile env m) => FilePath -> m a
decodeFile fp = do
  result <- decodeFileOrFail fp
  case result of
    Left (_, str) -> throwError . storeError $ "Decoding error: " <> toText str
    Right x -> pure x
{-# INLINE decodeFile #-}

decodeFileOrFail ::
  (Binary a, WithFile env m) => FilePath -> m (Either (ByteOffset, String) a)
decodeFileOrFail fp = do
  withBinaryFileDurableAtomic fp ReadMode $ \h ->
    feed (runGetIncremental Bin.get) h
  where
    feed dec h = case dec of
      (Done _ _ x) -> return (Right x)
      (Fail _ pos str) -> return (Left (pos, str))
      (Partial k) -> do
        chunk <- liftIO $ B.hGet h L.defaultChunkSize
        case B.length chunk of
          0 -> feed (k Nothing) h
          _ -> feed (k (Just chunk)) h

idToPath :: (WithFile env m) => Id a -> m FilePath
idToPath resId = do
  dataPath <- grab @DataPath
  let showId = show resId
  let fileEnding = ".bin"
  pure $ dataPath <> showId <> fileEnding

-- * Error helpers

extractionError :: (WithError m) => Maybe a -> m a
extractionError = withFrozenCallStack . throwOnNothing singleEntryError

singleEntryError :: AppErrorType
singleEntryError = storeError "Could not extract resource from file"
