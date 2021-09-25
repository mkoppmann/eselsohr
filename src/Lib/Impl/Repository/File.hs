module Lib.Impl.Repository.File
  ( WithFile
  , load
  , save
  , exists
  , init
  ,

    -- * Error helpers
    extractionError
  ) where

import qualified Codec.Serialise               as Ser
import           Codec.Serialise.Class          ( Serialise )
import           Lib.App                        ( AppErrorType
                                                , DataPath
                                                , Has
                                                , MaxConcurrentWrites
                                                , WithError
                                                , WriteQueue
                                                , grab
                                                , storeError
                                                , throwOnNothing
                                                )
import           Lib.Core.Domain                ( Id
                                                , StoreEvent
                                                , apply
                                                )
import           Prelude                 hiding ( init )
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Directory             ( doesFileExist )
import           UnliftIO.IO.File               ( writeBinaryFileDurableAtomic )

type WithFile env m
  = ( MonadReader env m
    , Has DataPath env
    , Has WriteQueue env
    , Has (Maybe MaxConcurrentWrites) env
    , MonadUnliftIO m
    )

load :: (Serialise a, WithError m, WithFile env m) => Id a -> (a -> b) -> m b
load resId getter = getter <$> (decodeFile =<< idToPath resId)

save :: (WithError m, WithFile env m) => Id a -> Seq StoreEvent -> m ()
save resId updates = do
  fp      <- idToPath resId
  content <- decodeFile fp
  let newContent = foldl' apply content updates
  encodeFile fp newContent

exists :: (WithFile env m) => Id a -> m Bool
exists resId = doesFileExist =<< idToPath resId

init :: (Serialise a, WithFile env m) => Id a -> a -> m ()
init resId val = flip encodeFile val =<< idToPath resId

encodeFile :: (Serialise a, WithFile env m) => FilePath -> a -> m ()
encodeFile fp = writeBinaryFileDurableAtomic fp . toStrict . Ser.serialise
{-# INLINE encodeFile #-}

decodeFile :: (Serialise a, WithFile env m) => FilePath -> m a
decodeFile fp = Ser.deserialise . fromStrict <$> liftIO (readFileBS fp)
{-# INLINE decodeFile #-}

idToPath :: (WithFile env m) => Id a -> m FilePath
idToPath resId = do
  dataPath <- grab @DataPath
  pure $ dataPath <> show resId <> ".bin"

-- * Error helpers

extractionError :: (WithError m) => Maybe a -> m a
extractionError = withFrozenCallStack . throwOnNothing singleEntryError

singleEntryError :: AppErrorType
singleEntryError = storeError "Could not extract resource from file"
