module Lib.Infra.Persistence.File
  ( WithFile
  , exists
  , load
  , init
  , save
  , decodeFile
  , encodeFile
  ) where


import qualified Codec.Compression.GZip                              as GZip

import           Data.Aeson                                           ( decode
                                                                      , encode
                                                                      )
import           Prelude                                       hiding ( init )
import           UnliftIO                                             ( MonadUnliftIO )
import           UnliftIO.Directory                                   ( doesFileExist )
import           UnliftIO.IO.File                                     ( writeBinaryFileDurableAtomic )

import           Data.Aeson.Types                                     ( FromJSON
                                                                      , ToJSON
                                                                      )
import           Lib.App.Env                                          ( DataPath
                                                                      , Has
                                                                      , grab
                                                                      )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Infra.Persistence.Model.Collection               ( CollectionPm )

type WithFile env m = (MonadReader env m, Has DataPath env, MonadUnliftIO m)

exists :: (WithFile env m) => Id Collection -> m Bool
exists colId = doesFileExist =<< idToPath colId

load :: (FromJSON a, WithFile env m) => Id Collection -> (a -> b) -> m b
load colId getter = do
  filePath <- idToPath colId
  colWm    <- decodeFile filePath
  pure $ getter colWm

init :: (ToJSON a, WithFile env m) => Id Collection -> a -> m ()
init colId val = flip encodeFile val =<< idToPath colId

save :: (WithFile env m) => Id Collection -> CollectionPm -> m ()
save colId col = do
  filePath <- idToPath colId
  encodeFile filePath col

decodeFile :: (FromJSON a, MonadIO m) => FilePath -> m a
decodeFile fp = do
  mContent <- decode . GZip.decompress <$> readFileLBS fp
  maybe (error "Could not decode file") pure mContent
{-# INLINE decodeFile #-}

encodeFile :: (ToJSON a, MonadIO m) => FilePath -> a -> m ()
encodeFile fp = writeBinaryFileDurableAtomic fp . toStrict . GZip.compress . encode
{-# INLINE encodeFile #-}

idToPath :: (WithFile env m) => Id Collection -> m FilePath
idToPath colId = do
  dataPath <- grab @DataPath
  pure $ dataPath <> show colId <> ".json.gz"
