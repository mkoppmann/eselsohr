module Lib.Infra.Persistence.File
  ( WithFile
  , exists
  , load
  , init
  , save
  , decodeFile
  , encodeFile
  , allIdsInPath
  ) where


import qualified Codec.Compression.GZip                              as GZip
import qualified Data.Sequence                                       as Seq
import qualified Data.Text                                           as T

import           Data.Aeson                                           ( decode
                                                                      , encode
                                                                      )
import           Data.Aeson.Types                                     ( FromJSON
                                                                      , ToJSON
                                                                      )
import           Prelude                                       hiding ( init )
import           UnliftIO                                             ( MonadUnliftIO )
import           UnliftIO.Directory                                   ( doesFileExist
                                                                      , listDirectory
                                                                      )
import           UnliftIO.IO.File                                     ( writeBinaryFileDurableAtomic )

import qualified Lib.Domain.Id                                       as Id

import           Lib.App.Env                                          ( DataPath
                                                                      , Has
                                                                      , grab
                                                                      )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Error                                     ( serverError )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Infra.Error                                      ( WithError
                                                                      , throwOnNothing
                                                                      )
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

allIdsInPath :: (WithError m, WithFile env m) => m (Seq (Id Collection))
allIdsInPath = do
  dataPath <- grab @DataPath
  paths    <- (dataPath <>) <<$>> listDirectory dataPath
  traverse pathToId $ Seq.fromList paths

idToPath :: (WithFile env m) => Id Collection -> m FilePath
idToPath colId = do
  dataPath <- grab @DataPath
  pure $ dataPath <> show colId <> fileEnding

pathToId :: (WithError m, WithFile env m) => FilePath -> m (Id Collection)
pathToId fp = do
  dataPath           <- grab @DataPath
  strippedSuffixPath <- throwOnNothing invalidSuffixError strippedSuffixPathM
  strippedPath       <- throwOnNothing invalidPrefixError $ strippedPathM dataPath strippedSuffixPath
  throwOnNothing invalidNameError $ Id.fromText strippedPath
 where
  strippedSuffixPathM = T.stripSuffix (toText fileEnding) (toText fp)
  strippedPathM prefix = T.stripPrefix (toText prefix)
  invalidSuffixError = serverError "Could not convert filepath to collection id"
  invalidPrefixError = serverError "Could not strip data path prefix"
  invalidNameError   = serverError "Filepath is not a valid collection id"

fileEnding :: String
fileEnding = ".json.gz"
