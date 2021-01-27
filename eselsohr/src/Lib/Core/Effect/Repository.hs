module Lib.Core.Effect.Repository
  ( -- * Access to data in a 'Resource'.
    ReadCapabilities (..),
    ReadEntity (..),
    Persist (..),
    RWCapabilities,
    RWEntity,

    -- * Pure setters
    Impl.insertCap,
    Impl.updateCap,
    Impl.deleteCap,
    Impl.insertAct,
    Impl.updateAct,
    Impl.deleteAct,
    Impl.insertArt,
    Impl.updateArt,
    Impl.deleteArt,

    -- * Init helpers
    systemColId,
    initialCapId,

    -- * 'Article' helpers
    artUpdateTitle,
    artUpdateState,
  )
where

import Lib.App (App)
import Lib.Core.Domain.Article (Article, ArticleState)
import qualified Lib.Core.Domain.Article as Article
import Lib.Core.Domain.Capability (Action (..), Capability (..))
import Lib.Core.Domain.Entity (Entity (..))
import qualified Lib.Core.Domain.Entity as Entity
import Lib.Core.Domain.Id (Id, mkNilId)
import Lib.Core.Domain.Resource (Resource)
import Lib.Core.Domain.StoreEvent (StoreEvent)
import qualified Lib.Impl.Repository as Impl
import UnliftIO (MonadUnliftIO)

class (MonadUnliftIO m) => ReadCapabilities m where
  getOneCap :: Id Resource -> Id Capability -> m (Entity Capability)
  getManyCap :: Id Resource -> [Id Capability] -> m (Map (Id Capability) Capability)
  lookupCap :: Id Resource -> Id Capability -> m (Maybe (Entity Capability))

  getOneAct :: Id Resource -> Id Action -> m (Entity Action)
  getManyAct :: Id Resource -> [Id Action] -> m (Map (Id Action) Action)
  lookupAct :: Id Resource -> Id Action -> m (Maybe (Entity Action))

  getAllCap :: Id Resource -> m (Map (Id Capability) Capability)
  getAllAct :: Id Resource -> m (Map (Id Action) Action)

  getCapIdForActId :: Id Resource -> Id Action -> m (Id Capability)

class (MonadUnliftIO m) => Persist m where
  init :: Id Resource -> m ()
  commit :: Id Resource -> Seq StoreEvent -> m ()

type RWCapabilities m = (ReadCapabilities m, Persist m)

instance ReadCapabilities App where
  getOneCap = Impl.getOne Impl.capabilityGetter
  getManyCap = Impl.getMany Impl.capabilityGetter
  lookupCap = Impl.lookup Impl.capabilityGetter

  getOneAct = Impl.getOne Impl.actionGetter
  getManyAct = Impl.getMany Impl.actionGetter
  lookupAct = Impl.lookup Impl.actionGetter

  getAllCap = Impl.getAll Impl.capabilityGetter
  getAllAct = Impl.getAll Impl.actionGetter

  getCapIdForActId = Impl.getCapIdForActId

instance Persist App where
  init colId = Impl.init colId Impl.articleColInit
  commit = Impl.commit

class (ReadCapabilities m) => ReadEntity a m where
  getOneEnt :: Id Resource -> Id a -> m (Entity a)
  getAllEnt :: Id Resource -> m (Map (Id a) a)
  lookupEnt :: Id Resource -> Id a -> m (Maybe (Entity a))

type RWEntity a m = (ReadEntity a m, Persist m)

instance ReadEntity Article App where
  getOneEnt = Impl.getOne Impl.articleGetter
  getAllEnt = Impl.getAll Impl.articleGetter
  lookupEnt = Impl.lookup Impl.articleGetter

-- Helper

artUpdateTitle ::
  (ReadEntity Article m) =>
  Id Resource ->
  Id Article ->
  Text ->
  m StoreEvent
artUpdateTitle resId aId aTitle = do
  art <- Entity.val <$> getOneEnt resId aId
  pure . Impl.updateArt aId $ art {Article.title = aTitle}

artUpdateState ::
  (ReadEntity Article m) =>
  Id Resource ->
  Id Article ->
  ArticleState ->
  m StoreEvent
artUpdateState resId aId aState = do
  art <- Entity.val <$> getOneEnt resId aId
  pure . Impl.updateArt aId $ art {Article.state = aState}

-- * System access

systemColId :: Id Resource
systemColId = mkNilId

initialCapId :: Id Capability
initialCapId = mkNilId
