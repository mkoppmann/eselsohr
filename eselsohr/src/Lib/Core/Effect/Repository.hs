module Lib.Core.Effect.Repository
  ( -- * Access to a 'Resource'.
    SealedResource,
    ContextState (..),
    ReadState (..),
    WriteState (..),
    RWState,

    -- * Getters with exceptions
    getOneCap,
    getOneAct,
    getOneArt,
    getCapIdForActId,

    -- * Pure getters
    getManyCap,
    lookupCap,
    getManyAct,
    lookupAct,
    getManyArt,
    lookupArt,

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

    -- * 'Article' helpers
    artUpdateTitle,
    artUpdateState,
  )
where

import Lib.App (App, WithError)
import Lib.Core.Domain.Article (Article, ArticleState)
import qualified Lib.Core.Domain.Article as Article
import Lib.Core.Domain.Capability (Action (..), Capability (..))
import Lib.Core.Domain.Context (Context)
import Lib.Core.Domain.Entity (Entity (..))
import Lib.Core.Domain.Id (Id)
import Lib.Core.Domain.Resource (Resource)
import Lib.Core.Domain.StoreEvent (StoreEvent)
import qualified Lib.Impl.Repository as Impl

newtype SealedResource = SealedResource {unSealResource :: Resource}

data ContextState = ContextState
  { csContext :: !Context,
    csResource :: !SealedResource
  }

class (Monad m) => ReadState m where
  load :: Id Resource -> m SealedResource

class (Monad m) => WriteState m where
  init :: Id Resource -> m ()
  commit :: Id Resource -> Seq StoreEvent -> m ()

type RWState m = (ReadState m, WriteState m)

instance ReadState App where
  load = pure . SealedResource <=< Impl.load

instance WriteState App where
  init colId = Impl.init colId Impl.articleColInit
  commit = Impl.commit

getOneCap ::
  (WithError m) => SealedResource -> Id Capability -> m (Entity Capability)
getOneCap sRes = Impl.getOne Impl.capabilityGetter (unSealResource sRes)

getManyCap ::
  SealedResource -> HashSet (Id Capability) -> HashMap (Id Capability) Capability
getManyCap sRes = Impl.getMany Impl.capabilityGetter (unSealResource sRes)

lookupCap ::
  SealedResource -> Id Capability -> Maybe (Entity Capability)
lookupCap sRes = Impl.lookup Impl.capabilityGetter (unSealResource sRes)

getOneAct ::
  (WithError m) => SealedResource -> Id Action -> m (Entity Action)
getOneAct sRes = Impl.getOne Impl.actionGetter (unSealResource sRes)

getManyAct ::
  SealedResource -> HashSet (Id Action) -> HashMap (Id Action) Action
getManyAct sRes = Impl.getMany Impl.actionGetter (unSealResource sRes)

lookupAct ::
  SealedResource -> Id Action -> Maybe (Entity Action)
lookupAct sRes = Impl.lookup Impl.actionGetter (unSealResource sRes)

getCapIdForActId ::
  (WithError m) => SealedResource -> Id Action -> m (Id Capability)
getCapIdForActId sRes = Impl.getCapIdForActId (unSealResource sRes)

getOneArt ::
  (WithError m) => SealedResource -> Id Article -> m (Entity Article)
getOneArt sRes = Impl.getOne Impl.articleGetter (unSealResource sRes)

getManyArt ::
  SealedResource -> HashSet (Id Article) -> HashMap (Id Article) Article
getManyArt sRes = Impl.getMany Impl.articleGetter (unSealResource sRes)

lookupArt ::
  SealedResource -> Id Article -> Maybe (Entity Article)
lookupArt sRes = Impl.lookup Impl.articleGetter (unSealResource sRes)

-- Helper

artUpdateTitle :: Id Article -> Text -> StoreEvent
artUpdateTitle aId aTitle =
  Impl.updateArt aId $ \oldArt -> oldArt {Article.title = aTitle}

artUpdateState :: Id Article -> ArticleState -> StoreEvent
artUpdateState aId aState =
  Impl.updateArt aId $ \oldArt -> oldArt {Article.state = aState}
