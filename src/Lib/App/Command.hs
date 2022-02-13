module Lib.App.Command
  (
    -- * Article related

    -- ** CreateArticle
    CreateArticle(..)
  , createArticle

    -- ** ChangeArticleTitle
  , ChangeArticleTitle(..)
  , changeArticleTitle

    -- ** ChangeArticleState
  , MarkArticleAsRead(..)
  , markArticleAsRead
  , MarkArticleAsUnread(..)
  , markArticleAsUnread

    -- ** DeleteArticle
  , DeleteArticle(..)
  , deleteArticle

    -- * Capability related

    -- ** CreateUnlockLink
  , CreateUnlockLink(..)
  , createUnlockLink

    -- ** DeleteUnlockLink
  , DeleteUnlockLink(..)
  , deleteUnlockLink

    -- ** AddShareUnlockLinks
  , AddShareUnlockLinks(..)
  , addShareUnlockLinks

    -- ** DeleteShareUnlockLinks
  , DeleteShareUnlockLinks(..)
  , deleteShareUnlockLinks

    -- ** AddShareArticleList
  , AddShareArticleList(..)
  , addShareArticleList

    -- ** DeleteShareArticleList
  , DeleteShareArticleList(..)
  , deleteShareArticleList

    -- ** AddShareArticle
  , AddShareArticle(..)
  , addShareArticle

    -- ** DeleteShareArticle
  , DeleteShareArticle(..)
  , deleteShareArticle

    -- * Collection related

    -- ** CreateCollection
  , createCollection
  ) where

import           Prelude                                       hiding ( id
                                                                      , state
                                                                      )

import qualified Lib.App.Port                                        as Port
import qualified Lib.Domain.Article                                  as Art
import qualified Lib.Domain.Authorization                            as Authz
import qualified Lib.Domain.Capability                               as Cap
import qualified Lib.Domain.Repo.ArticleList                         as ArtRepo
import qualified Lib.Domain.Repo.CapabilityList                      as CapRepo
import qualified Lib.Domain.Repo.Collection                          as ColRepo

import           Data.Time.Clock                                      ( UTCTime )
import           Lib.App.Port                                         ( MonadRandom
                                                                      , MonadScraper
                                                                      , MonadTime
                                                                      )
import           Lib.Domain.Article                                   ( Article
                                                                      , ArticleState
                                                                      )
import           Lib.Domain.Capability                                ( ArticlePerms
                                                                      , ArticlesPerms
                                                                      , Capability
                                                                      , ObjectReference
                                                                      , OverviewPerms
                                                                      )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Error                                     ( AppErrorType )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Repo.ArticleList                          ( ArticleListAction
                                                                      , ArticleListRepo
                                                                      )
import           Lib.Domain.Repo.CapabilityList                       ( CapabilityListRepo )
import           Lib.Domain.Repo.Collection                           ( CollectionRepo )
import           Lib.Domain.Uri                                       ( Uri )

type CommandResult = Either AppErrorType ()

------------------------------------------------------------------------
-- CreateArticle
------------------------------------------------------------------------

data CreateArticle = CreateArticle
  { colId  :: !(Id Collection)
  , uri    :: !Uri
  , objRef :: !ObjectReference
  }

createArticle :: (ArticleListRepo m, MonadScraper m, MonadTime m) => CreateArticle -> m CommandResult
createArticle CreateArticle {..} = do
  artId    <- ArtRepo.nextId
  title    <- Port.scrapWebsite uri
  creation <- Port.getCurrentTime
  case mkAction artId title creation of
    Left  err    -> pure $ Left err
    Right action -> Right <$> ArtRepo.save colId action
 where
  mkAction :: Id Article -> Text -> UTCTime -> Either AppErrorType ArticleListAction
  mkAction id artTitle creation = do
    perm  <- Authz.canCreateArticles objRef
    title <- Art.titleFromText artTitle
    let state = Art.Unread
        art   = Art.Article { .. }
    pure $ ArtRepo.addArticle perm id art

------------------------------------------------------------------------
-- ChangeArticleTitle
------------------------------------------------------------------------

data ChangeArticleTitle = ChangeArticleTitle
  { colId  :: !(Id Collection)
  , artId  :: !(Id Article)
  , title  :: !Text
  , objRef :: !ObjectReference
  }

changeArticleTitle :: (ArticleListRepo m) => ChangeArticleTitle -> m CommandResult
changeArticleTitle ChangeArticleTitle {..} = case mkAction of
  Left  err    -> pure $ Left err
  Right action -> Right <$> ArtRepo.save colId action
 where
  mkAction :: Either AppErrorType ArticleListAction
  mkAction = do
    perm     <- Authz.canChangeArticleTitle objRef artId
    artTitle <- Art.titleFromText title
    pure $ ArtRepo.changeArticleTitle perm artTitle

------------------------------------------------------------------------
-- ChangeArticleState
------------------------------------------------------------------------

data MarkArticleAsRead = MarkArticleAsRead
  { colId  :: !(Id Collection)
  , artId  :: !(Id Article)
  , objRef :: !ObjectReference
  }

markArticleAsRead :: (ArticleListRepo m) => MarkArticleAsRead -> m CommandResult
markArticleAsRead MarkArticleAsRead {..} = changeArticleState colId artId objRef Art.Read

data MarkArticleAsUnread = MarkArticleAsUnread
  { colId  :: !(Id Collection)
  , artId  :: !(Id Article)
  , objRef :: !ObjectReference
  }

markArticleAsUnread :: (ArticleListRepo m) => MarkArticleAsUnread -> m CommandResult
markArticleAsUnread MarkArticleAsUnread {..} = changeArticleState colId artId objRef Art.Unread

changeArticleState
  :: (ArticleListRepo m) => Id Collection -> Id Article -> ObjectReference -> ArticleState -> m CommandResult
changeArticleState colId artId objRef artState = case Authz.canChangeArticleState objRef artId of
  Left  err  -> pure $ Left err
  Right perm -> Right <$> ArtRepo.save colId (action perm artState)
 where
  action perm Art.Unread = ArtRepo.markArticleAsUnread perm
  action perm Art.Read   = ArtRepo.markArticleAsRead perm

------------------------------------------------------------------------
-- DeleteArticle
------------------------------------------------------------------------

data DeleteArticle = DeleteArticle
  { colId  :: !(Id Collection)
  , artId  :: !(Id Article)
  , objRef :: !ObjectReference
  }

deleteArticle :: (ArticleListRepo m) => DeleteArticle -> m CommandResult
deleteArticle DeleteArticle {..} = case Authz.canDeleteArticle objRef artId of
  Left  err  -> pure $ Left err
  Right perm -> Right <$> ArtRepo.save colId (ArtRepo.removeArticle perm)

------------------------------------------------------------------------
-- CreateUnlockLink
------------------------------------------------------------------------

data CreateUnlockLink = CreateUnlockLink
  { colId    :: !(Id Collection)
  , mPetname :: !(Maybe Text)
  , mExpDate :: !(Maybe UTCTime)
  , objRef   :: !ObjectReference
  }

createUnlockLink :: (CapabilityListRepo m) => CreateUnlockLink -> m CommandResult
createUnlockLink CreateUnlockLink {..} = do
  capId <- CapRepo.nextId
  let cap = Cap.mkCapability capId Cap.defaultArticlesRef mPetname mExpDate
  case Authz.canCreateUnlockLinks objRef of
    Left  err  -> pure $ Left err
    Right perm -> Right <$> CapRepo.save colId (CapRepo.addUnlockLink perm capId cap)

------------------------------------------------------------------------
-- DeleteUnlockLink
------------------------------------------------------------------------

data DeleteUnlockLink = DeleteUnlockLink
  { colId  :: !(Id Collection)
  , capId  :: !(Id Capability)
  , objRef :: !ObjectReference
  }

deleteUnlockLink :: (CapabilityListRepo m) => DeleteUnlockLink -> m CommandResult
deleteUnlockLink DeleteUnlockLink {..} = case Authz.canDeleteUnlockLinks objRef of
  Left  err  -> pure $ Left err
  Right perm -> Right <$> CapRepo.save colId (CapRepo.removeUnlockLink perm capId)

------------------------------------------------------------------------
-- AddShareUnlockLink
------------------------------------------------------------------------

data AddShareUnlockLinks = AddShareUnlockLinks
  { colId       :: !(Id Collection)
  , sharedPerms :: !OverviewPerms
  , mPetname    :: !(Maybe Text)
  , mExpDate    :: !(Maybe UTCTime)
  , objRef      :: !ObjectReference
  }

addShareUnlockLinks :: (CapabilityListRepo m) => AddShareUnlockLinks -> m CommandResult
addShareUnlockLinks AddShareUnlockLinks {..} = do
  capId <- CapRepo.nextId
  case Authz.canShareUnlockLinks objRef of
    Left  err  -> pure $ Left err
    Right perm -> case mkAction capId perm of
      Nothing     -> pure Authz.unauthorized
      Just action -> Right <$> CapRepo.save colId action
 where
  mkAction capId perm = do
    sharedObjRef <- Cap.createSharedOverviewRef objRef sharedPerms
    let cap = Cap.mkCapability capId sharedObjRef mPetname mExpDate
    pure $ CapRepo.addShareUnlockLinks perm capId cap

------------------------------------------------------------------------
-- DeleteShareUnlockLink
------------------------------------------------------------------------

data DeleteShareUnlockLinks = DeleteShareUnlockLinks
  { colId  :: !(Id Collection)
  , capId  :: !(Id Capability)
  , objRef :: !ObjectReference
  }

deleteShareUnlockLinks :: (CapabilityListRepo m) => DeleteShareUnlockLinks -> m CommandResult
deleteShareUnlockLinks DeleteShareUnlockLinks {..} = case Authz.canShareUnlockLinks objRef of
  Left  err  -> pure $ Left err
  Right perm -> Right <$> CapRepo.save colId (CapRepo.removeShareUnlockLinks perm capId)

------------------------------------------------------------------------
-- AddShareArticleList
------------------------------------------------------------------------

data AddShareArticleList = AddShareArticleList
  { colId       :: !(Id Collection)
  , sharedPerms :: !ArticlesPerms
  , mPetname    :: !(Maybe Text)
  , mExpDate    :: !(Maybe UTCTime)
  , objRef      :: !ObjectReference
  }

addShareArticleList :: (CapabilityListRepo m) => AddShareArticleList -> m CommandResult
addShareArticleList AddShareArticleList {..} = do
  capId <- CapRepo.nextId
  case Authz.canShareArticleList objRef of
    Left  err  -> pure $ Left err
    Right perm -> case mkAction capId perm of
      Nothing     -> pure Authz.unauthorized
      Just action -> Right <$> CapRepo.save colId action
 where
  mkAction capId perm = do
    sharedObjRef <- Cap.createSharedArticlesRef objRef sharedPerms
    let cap = Cap.mkCapability capId sharedObjRef mPetname mExpDate
    pure $ CapRepo.addShareArticleList perm capId cap

------------------------------------------------------------------------
-- DeleteShareArticleList
------------------------------------------------------------------------

data DeleteShareArticleList = DeleteShareArticleList
  { colId  :: !(Id Collection)
  , capId  :: !(Id Capability)
  , objRef :: !ObjectReference
  }

deleteShareArticleList :: (CapabilityListRepo m) => DeleteShareArticleList -> m CommandResult
deleteShareArticleList DeleteShareArticleList {..} = case Authz.canShareArticleList objRef of
  Left  err  -> pure $ Left err
  Right perm -> Right <$> CapRepo.save colId (CapRepo.removeShareArticleList perm capId)

------------------------------------------------------------------------
-- AddShareArticle
------------------------------------------------------------------------

data AddShareArticle = AddShareArticle
  { colId       :: !(Id Collection)
  , artId       :: !(Id Article)
  , sharedPerms :: !ArticlePerms
  , mPetname    :: !(Maybe Text)
  , mExpDate    :: !(Maybe UTCTime)
  , objRef      :: !ObjectReference
  }

addShareArticle :: (CapabilityListRepo m) => AddShareArticle -> m CommandResult
addShareArticle AddShareArticle {..} = do
  capId <- CapRepo.nextId
  case Authz.canShareArticle objRef artId of
    Left  err  -> pure $ Left err
    Right perm -> case mkAction capId perm of
      Nothing     -> pure Authz.unauthorized
      Just action -> Right <$> CapRepo.save colId action
 where
  mkAction capId perm = do
    sharedObjRef <- Cap.createSharedArticleRef objRef sharedPerms artId
    let cap = Cap.mkCapability capId sharedObjRef mPetname mExpDate
    pure $ CapRepo.addShareArticle perm capId cap

------------------------------------------------------------------------
-- DeleteShareArticle
------------------------------------------------------------------------

data DeleteShareArticle = DeleteShareArticle
  { colId  :: !(Id Collection)
  , artId  :: !(Id Article)
  , capId  :: !(Id Capability)
  , objRef :: !ObjectReference
  }

deleteShareArticle :: (CapabilityListRepo m) => DeleteShareArticle -> m CommandResult
deleteShareArticle DeleteShareArticle {..} = case Authz.canShareArticle objRef artId of
  Left  err  -> pure $ Left err
  Right perm -> Right <$> CapRepo.save colId (CapRepo.removeShareArticle perm capId)

------------------------------------------------------------------------
-- CreateCollection
------------------------------------------------------------------------

createCollection :: (CollectionRepo m, MonadRandom m) => m (Id Collection, Id Capability)
createCollection = do
  colId                   <- Port.getRandomId
  collectionAlreadyExists <- ColRepo.exists colId
  if collectionAlreadyExists then createCollection else createCollection' colId
 where
  createCollection' :: (CollectionRepo m, MonadRandom m) => Id Collection -> m (Id Collection, Id Capability)
  createCollection' colId = do
    capId <- Port.getRandomId
    let cap = Cap.mkCapability capId Cap.defaultOverviewRef Nothing Nothing
    ColRepo.createCollection colId capId cap
    pure (colId, capId)
