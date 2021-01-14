module Lib.Init
  ( datafolder,
    initialCap,
  )
where

import qualified Data.Binary as Bin
import qualified Data.Map.Strict as Map
import qualified Data.UUID.V4 as UUID
import Lib.Core.Domain (Action (..), Capability (..), CapabilityCollection (..), CommandAction (..), FrontendAction (..), Id, PostAction (..), QueryAction (..), Resource (..), ResourceOverviewActions (..))
import qualified Lib.Core.Domain.Id as Id
import qualified Lib.Core.Effect.Repository as R
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)

datafolder :: (MonadIO m) => FilePath -> m ()
datafolder = liftIO . createDirectoryIfMissing True

initialCap :: (MonadIO m) => FilePath -> m ()
initialCap fp =
  sysFileExists >>= \fExists -> if fExists then prepareAccess else initFile
  where
    sysFileExists :: (MonadIO m) => m Bool
    sysFileExists = liftIO $ doesFileExist sysFilePath

    initFile :: (MonadIO m) => m ()
    initFile = liftIO $ Bin.encodeFile sysFilePath =<< createInitialCap

    prepareAccess :: (MonadIO m) => m ()
    prepareAccess = flip when initFile =<< lookupInitialCap

    lookupInitialCap :: (MonadIO m) => m Bool
    lookupInitialCap = do
      resource <- liftIO $ Bin.decodeFile sysFilePath
      case resource of
        ArticleResource _ -> pure False -- throw an exception here?
        CapOnlyResource capCol -> pure . Map.null $ capCollection capCol

    createInitialCap :: (MonadIO m) => m Resource
    createInitialCap = do
      let command = Command $ Post CreateResource
      let query =
            Query
              . ResourceOverview
              $ ResourceOverviewActions
                Id.mkNilId
                Id.mkNilId
                Id.mkNilId
                Id.mkNilId
                Nothing
                Nothing
      let redirectTo = "Change Me"
      (cId, cMap) <- storeInMap Nothing command Map.empty
      (qId, cqMap) <- storeInMap Nothing query cMap
      let frontend = Frontend $ FrontendAction cId qId redirectTo
      (fId, actMap) <- storeInMap Nothing frontend cqMap

      let cap = Capability Nothing Nothing fId
      (_, capMap) <- storeInMap (Just R.initialCapId) cap Map.empty
      pure . CapOnlyResource $ CapabilityCollection capMap actMap

    storeInMap ::
      (MonadIO m) =>
      Maybe (Id a) ->
      a ->
      Map (Id a) a ->
      m (Id a, Map (Id a) a)
    storeInMap mKey val aMap = case mKey of
      Just key -> pure (key, Map.insert key val aMap)
      Nothing -> do
        aId <- Id.Id <$> liftIO UUID.nextRandom
        pure (aId, Map.insert aId val aMap)

    sysFilePath :: FilePath
    sysFilePath =
      let fId = toString $ Id.toText R.systemColId
       in fp <> fId <> ".bin"
