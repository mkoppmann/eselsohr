module Lib.Impl.Repository
  ( -- * Generic implementations
    getOne,
    getMany,
    lookup,
    init,
    insert,
    update,
    delete,
    getAll,
    getCapIdForActId,

    -- * 'Resource' specific getters and setters
    articleGetter,
    articleSetter,
    artUpdater,
    articleColInit,
    capabilityGetter,
    capabilitySetter,
    capabilityColInit,
    capUpdater,
    actionGetter,
    actionSetter,
    actUpdater,
    -- Error helper
    asSingleEntry,
  )
where

import qualified Data.Map.Strict as Map
import Lib.App (AppErrorType, WithError, storeError, throwOnNothing)
import Lib.Core.Domain.Article (Article (..))
import qualified Lib.Core.Domain.Article as Article
import Lib.Core.Domain.Capability (Action, Capability (..))
import Lib.Core.Domain.Entity (Entity (..))
import Lib.Core.Domain.Id (Id)
import Lib.Core.Domain.Resource (ArticleCollection (..), CapabilityCollection (..), Resource (..))
import Lib.Impl.Repository.File (WithFile)
import qualified Lib.Impl.Repository.File as File
import Prelude hiding (getAll, init)

type CollectionGetter a = (Resource -> Maybe (Map (Id a) a))

type CollectionSetter a = (Resource -> Map (Id a) a -> Resource)

getOne ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  Id Resource ->
  Id a ->
  m (Entity a)
getOne getter resId = asSingleEntry <=< lookup getter resId

getMany ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  Id Resource ->
  [Id a] ->
  m (Map (Id a) a)
getMany getter resId entIds =
  Map.filterWithKey (\key _ -> key `elem` entIds) <$> getAll getter resId

lookup ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  Id Resource ->
  Id a ->
  m (Maybe (Entity a))
lookup getter resId entId = do
  aMap <- getAll getter resId
  let mIx = Map.lookupIndex entId aMap
  let mTuple = fmap (`Map.elemAt` aMap) mIx
  pure $ fmap (uncurry Entity) mTuple

init ::
  (WithFile env m) =>
  Id Resource ->
  Resource ->
  m ()
init = File.init

insert ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  CollectionSetter a ->
  Id Resource ->
  Id a ->
  a ->
  m ()
insert getter setter resId entId newEnt = do
  File.save resId (gsetter getter setter (uncurry Map.insert)) (entId, newEnt)

update ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  CollectionSetter a ->
  (a -> a) ->
  Id Resource ->
  Id a ->
  a ->
  m ()
update getter setter updater resId entId =
  File.save resId (gsetter getter setter setter')
  where
    setter' _ oldRes = Map.adjust updater entId oldRes

delete ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  CollectionSetter a ->
  Id Resource ->
  Id a ->
  m ()
delete getter setter resId = File.save resId (gsetter getter setter Map.delete)

getAll ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  Id Resource ->
  m (Map (Id a) a)
getAll = flip File.load

getCapIdForActId ::
  (WithError m, WithFile env m) =>
  Id Resource ->
  Id Action ->
  m (Id Capability)
getCapIdForActId resId actId = do
  allCaps <- getAll capabilityGetter resId
  let capList = Map.toList $ Map.filter ((==) actId . actionId) allCaps
  let capIdList = fst <$> capList
  asSingleEntry $ viaNonEmpty head capIdList

gsetter ::
  CollectionGetter a ->
  CollectionSetter a ->
  (b -> Map (Id a) a -> Map (Id a) a) ->
  Resource ->
  b ->
  Resource
gsetter fromCol toCol setter oldRes val =
  maybe oldRes (toCol oldRes . setter val) $ fromCol oldRes

-- | Only the title and the state of an 'Article' can be changed.
artUpdater ::
  (WithError m, WithFile env m) =>
  Id Resource ->
  Id Article ->
  Article ->
  m ()
artUpdater resId artId newArt =
  update articleGetter articleSetter updateArt resId artId newArt
  where
    updateArt oldArt = oldArt {Article.title = newTitle, Article.state = newState}
    newTitle = Article.title newArt
    newState = Article.state newArt

capUpdater ::
  (WithError m, WithFile env m) =>
  Id Resource ->
  Id Capability ->
  Capability ->
  m ()
capUpdater resId capId newCap =
  update capabilityGetter capabilitySetter updateCap resId capId newCap
  where
    updateCap _ = newCap

actUpdater ::
  (WithError m, WithFile env m) =>
  Id Resource ->
  Id Action ->
  Action ->
  m ()
actUpdater resId actId newAct =
  update actionGetter actionSetter updateAct resId actId newAct
  where
    updateAct _ = newAct

-- * 'Resource' specific getters and setters

articleGetter :: CollectionGetter Article
articleGetter = \case
  ArticleResource artCol -> Just $ artCollection artCol
  CapOnlyResource _nonArticleResource -> Nothing

articleSetter :: CollectionSetter Article
articleSetter oldRes newVal = case oldRes of
  ArticleResource artCol -> ArticleResource $ artCol {artCollection = newVal}
  CapOnlyResource _nonArticleResource -> oldRes

articleColInit :: Resource
articleColInit =
  ArticleResource $ ArticleCollection Map.empty Map.empty Map.empty

capabilityGetter :: CollectionGetter Capability
capabilityGetter = \case
  ArticleResource artCol -> Just $ artCapCollection artCol
  CapOnlyResource capCol -> Just $ capCollection capCol

capabilitySetter :: CollectionSetter Capability
capabilitySetter oldRes newVal = case oldRes of
  ArticleResource artCol -> ArticleResource $ artCol {artCapCollection = newVal}
  CapOnlyResource capCol -> CapOnlyResource $ capCol {capCollection = newVal}

capabilityColInit :: Resource
capabilityColInit = CapOnlyResource $ CapabilityCollection Map.empty Map.empty

actionGetter :: CollectionGetter Action
actionGetter = \case
  ArticleResource artCol -> Just $ artActCollection artCol
  CapOnlyResource capCol -> Just $ capActCollection capCol

actionSetter :: CollectionSetter Action
actionSetter oldRes newVal = case oldRes of
  ArticleResource artCol -> ArticleResource $ artCol {artActCollection = newVal}
  CapOnlyResource capCol -> CapOnlyResource $ capCol {capActCollection = newVal}

-- Error helpers

asSingleEntry :: (WithError m) => Maybe a -> m a
asSingleEntry = withFrozenCallStack . throwOnNothing singleEntryError

singleEntryError :: AppErrorType
singleEntryError = storeError "Expected a single entry, but got none"
