module Lib.Core.Effect.Repository
  ( -- * Access to the 'Capability's and 'Action's in a 'Resource'.
    ReadCapabilities (..),
    WriteCapabilities (..),
    RWCapabilities,

    -- * Access to the 'Entity' of a type @a@ in a 'Resource'.
    ReadEntity (..),
    WriteEntity (..),
    RWEntity,

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

class (MonadUnliftIO m) => WriteCapabilities m where
  insertCap :: Id Resource -> Id Capability -> Capability -> m ()
  updateCap :: Id Resource -> Id Capability -> Capability -> m ()
  deleteCap :: Id Resource -> Id Capability -> m ()

  insertAct :: Id Resource -> Id Action -> Action -> m ()
  updateAct :: Id Resource -> Id Action -> Action -> m ()
  deleteAct :: Id Resource -> Id Action -> m ()

type RWCapabilities m = (ReadCapabilities m, WriteCapabilities m)

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

instance WriteCapabilities App where
  insertCap = Impl.insert Impl.capabilityGetter Impl.capabilitySetter
  updateCap = Impl.capUpdater
  deleteCap = Impl.delete Impl.capabilityGetter Impl.capabilitySetter

  insertAct = Impl.insert Impl.actionGetter Impl.actionSetter
  updateAct = Impl.actUpdater
  deleteAct = Impl.delete Impl.actionGetter Impl.actionSetter

class (ReadCapabilities m) => ReadEntity a m where
  getOneEnt :: Id Resource -> Id a -> m (Entity a)
  getAllEnt :: Id Resource -> m (Map (Id a) a)
  lookupEnt :: Id Resource -> Id a -> m (Maybe (Entity a))

class (WriteCapabilities m) => WriteEntity a m where
  initRes :: Id Resource -> Maybe a -> m ()
  insertEnt :: Id Resource -> Id a -> a -> m ()
  updateEnt :: Id Resource -> Id a -> a -> m ()
  deleteEnt :: Id Resource -> Id a -> m ()

type RWEntity a m = (ReadEntity a m, WriteEntity a m)

instance ReadEntity Article App where
  getOneEnt = Impl.getOne Impl.articleGetter
  getAllEnt = Impl.getAll Impl.articleGetter
  lookupEnt = Impl.lookup Impl.articleGetter

instance WriteEntity Article App where
  initRes colId _ = Impl.init colId Impl.articleColInit
  insertEnt = Impl.insert Impl.articleGetter Impl.articleSetter
  updateEnt = Impl.artUpdater
  deleteEnt = Impl.delete Impl.articleGetter Impl.articleSetter

-- Helper

artUpdateTitle ::
  (RWEntity Article m) =>
  Id Resource ->
  Id Article ->
  Text ->
  m ()
artUpdateTitle resId aId aTitle = do
  art <- Entity.val <$> getOneEnt resId aId
  updateEnt resId aId $ art {Article.title = aTitle}

artUpdateState ::
  (RWEntity Article m) =>
  Id Resource ->
  Id Article ->
  ArticleState ->
  m ()
artUpdateState resId aId aState = do
  art <- Entity.val <$> getOneEnt resId aId
  updateEnt resId aId $ art {Article.state = aState}

-- * System access

systemColId :: Id Resource
systemColId = mkNilId

initialCapId :: Id Capability
initialCapId = mkNilId
