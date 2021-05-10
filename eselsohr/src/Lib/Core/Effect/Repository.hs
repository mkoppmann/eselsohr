module Lib.Core.Effect.Repository
  (
  -- * Access to a 'Resource'.
    SealedResource
  , ContextState(..)
  , ReadState(..)
  , WriteState(..)
  , RWState

  -- * Getter
  , getOneArt
  , lookupArt
  , getAllArt
  , getOneCap
  , lookupCap
  , getAllCap

  -- * Setter
  , insertArt
  , updateArtTitle
  , updateArtState
  , deleteArt
  , insertCap
  , updateCap
  , deleteCap

  -- * Direct access without authorization
  , noAuthLookupCap
  , noAuthInsertCap
  ) where

import           Lib.App                        ( App
                                                , WithError
                                                , throwError
                                                )
import qualified Lib.App.Error                 as Error
import           Lib.Core.Domain                ( Action(..)
                                                , Article
                                                , ArticleAction(..)
                                                , ArticleState
                                                , AuthAction(getAction)
                                                , Capability(..)
                                                , Context
                                                , Entity(..)
                                                , Id
                                                , OverviewAction(..)
                                                , Resource
                                                , StoreEvent
                                                )
import           Lib.Impl.Repository            ( SealedResource )
import qualified Lib.Impl.Repository           as Impl

data ContextState = ContextState
  { csContext  :: !Context
  , csResource :: !SealedResource
  }

class (Monad m) => ReadState m where
  load :: Id Resource -> m SealedResource

class (Monad m) => WriteState m where
  init :: Id Resource -> m ()
  commit :: Id Resource -> Seq StoreEvent -> m ()

type RWState m = (ReadState m, WriteState m)

instance ReadState App where
  load = Impl.load

instance WriteState App where
  init   = Impl.initArtCol
  commit = Impl.commit

getOneArt
  :: (WithError m) => SealedResource -> AuthAction -> m (Entity Article)
getOneArt sRes authAction = case getAction authAction of
  ArticleAct artAct -> case artAct of
    ViewArticle aId -> Impl.getOneArt sRes aId
    _otherAct       -> notAuthorized
  _otherAct -> notAuthorized

lookupArt
  :: (WithError m) => SealedResource -> AuthAction -> m (Maybe (Entity Article))
lookupArt sRes authAction = case getAction authAction of
  ArticleAct artAct -> case artAct of
    ViewArticle aId -> pure $ Impl.lookupArt sRes aId
    _otherAct       -> notAuthorized
  _otherAct -> notAuthorized

getAllArt
  :: (WithError m)
  => SealedResource
  -> AuthAction
  -> m (HashMap (Id Article) Article)
getAllArt sRes authAction = case getAction authAction of
  ArticleAct artAct -> case artAct of
    ViewArticles -> pure $ Impl.getAllArt sRes
    _otherAct    -> notAuthorized
  _otherAct -> notAuthorized

insertArt :: (WithError m) => AuthAction -> Article -> m StoreEvent
insertArt authAction art = case getAction authAction of
  ArticleAct artAct -> case artAct of
    CreateArticle aId -> pure $ Impl.insertArt aId art
    _otherAct         -> notAuthorized
  _otherAct -> notAuthorized

updateArtTitle :: (WithError m) => AuthAction -> Text -> m StoreEvent
updateArtTitle authAction aTitle = case getAction authAction of
  ArticleAct artAct -> case artAct of
    ChangeArticleTitle aId -> pure $ Impl.updateArtTitle aId aTitle
    _otherAct              -> notAuthorized
  _otherAct -> notAuthorized

updateArtState :: (WithError m) => AuthAction -> ArticleState -> m StoreEvent
updateArtState authAction aState = case getAction authAction of
  ArticleAct artAct -> case artAct of
    ChangeArticleState aId -> pure $ Impl.updateArtState aId aState
    _otherAct              -> notAuthorized
  _otherAct -> notAuthorized

deleteArt :: (WithError m) => AuthAction -> m StoreEvent
deleteArt authAction = case getAction authAction of
  ArticleAct artAct -> case artAct of
    DeleteArticle aId -> pure $ Impl.deleteArt aId
    _otherAct         -> notAuthorized
  _otherAct -> notAuthorized

getOneCap
  :: (WithError m) => SealedResource -> AuthAction -> m (Entity Capability)
getOneCap _sRes authAction = case getAction authAction of
  _otherAct -> notAuthorized

lookupCap
  :: (WithError m)
  => SealedResource
  -> AuthAction
  -> m (Maybe (Entity Capability))
lookupCap _sRes authAction = case getAction authAction of
  _otherAct -> notAuthorized

getAllCap
  :: (WithError m)
  => SealedResource
  -> AuthAction
  -> m (HashMap (Id Capability) Capability)
getAllCap sRes authAction = case getAction authAction of
  OverviewAct oAct -> case oAct of
    ViewUnlockLinks -> pure $ Impl.getAllCap sRes
    _otherAct       -> notAuthorized
  _otherAct -> notAuthorized

insertCap :: (WithError m) => AuthAction -> Capability -> m StoreEvent
insertCap authAction cap = case getAction authAction of
  OverviewAct oAct -> case oAct of
    CreateUnlockLink unlockLinkId -> pure $ Impl.insertCap unlockLinkId cap
    _otherAct                     -> notAuthorized
  _otherAct -> notAuthorized

updateCap
  :: (WithError m) => AuthAction -> (Capability -> Capability) -> m StoreEvent
updateCap authAction _capUpdater = case getAction authAction of
  _otherAct -> notAuthorized

deleteCap :: (WithError m) => AuthAction -> m StoreEvent
deleteCap authAction = case getAction authAction of
  OverviewAct oAct -> case oAct of
    DeleteUnlockLink unlockLinkId -> pure $ Impl.deleteCap unlockLinkId
    _otherAct                     -> notAuthorized
  _otherAct -> notAuthorized

noAuthLookupCap :: SealedResource -> Id Capability -> Maybe (Entity Capability)
noAuthLookupCap = Impl.lookupCap

noAuthInsertCap :: Id Capability -> Capability -> StoreEvent
noAuthInsertCap = Impl.insertCap

-- * Helpers

notAuthorized :: (WithError m) => m a
notAuthorized = throwError $ Error.notAuthorized
  "You are missing the required permissions for this action"
