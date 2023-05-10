module Test.Domain.Shared
    ( authorized
    , defaultArticle
    , defaultArticleWithId
    , defaultArticleWithState
    , getNonEmptyText
    , getUri
    , getCurrentTime
    , getRandomId
    , objRefWithAllOverviewPerms
    , objRefWithNoOverviewPerms
    , objRefWithAllArticlesPerms
    , objRefWithNoArticlesPerms
    , objRefWithAllArticlePerms
    , objRefWithNoArticlePerms
    ) where

import Data.Time (UTCTime)

import qualified Lib.Domain.Article as Article
import qualified Lib.Domain.Capability as Capability
import qualified Lib.Domain.NonEmptyText as NET
import qualified Lib.Domain.Uri as Uri
import qualified Lib.Infra.Adapter.Random as Adapter
import qualified Lib.Infra.Adapter.Time as Adapter

import Lib.Domain.Article
    ( Article
    , ArticleState
    )
import Lib.Domain.Capability (ObjectReference)
import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Id (Id)
import Lib.Domain.NonEmptyText (NonEmptyText)
import Lib.Domain.Uri (Uri)

authorized :: Either AppErrorType a -> a
authorized (Left err) = error $ "Error when authorizing action: " <> show err
authorized (Right perm) = perm

defaultArticle :: (MonadIO m) => m Article
defaultArticle = defaultArticleWithState Article.Unread

defaultArticleWithId :: (MonadIO m) => Id Article -> m Article
defaultArticleWithId artId = defaultArticleWithIdAndState artId Article.Unread

defaultArticleWithState :: (MonadIO m) => ArticleState -> m Article
defaultArticleWithState artState = do
    artId <- getRandomId
    defaultArticleWithIdAndState artId artState

defaultArticleWithIdAndState :: (MonadIO m) => Id Article -> ArticleState -> m Article
defaultArticleWithIdAndState artId artState = do
    artCreation <- getCurrentTime
    let artTitle = getNonEmptyText "Default title"
        artUri = getUri "http://www.example.org"
    pure $ Article.Article artId artTitle artUri artState artCreation

getNonEmptyText :: Text -> NonEmptyText
getNonEmptyText title = case NET.fromText title "" of
    Left _err -> error "Error when creating default title for article"
    Right artTitle -> artTitle

getUri :: Text -> Uri
getUri uri = case Uri.mkUri uri of
    Left err -> error $ "Error when creating default uri for article:" <> show err
    Right artUri -> artUri

getCurrentTime :: (MonadIO m) => m UTCTime
getCurrentTime = Adapter.getCurrentTime

getRandomId :: (MonadIO m) => m (Id a)
getRandomId = Adapter.getRandomId

objRefWithAllOverviewPerms :: ObjectReference
objRefWithAllOverviewPerms = Capability.OverviewRef $ Capability.mkOverviewPerms canView canCreate canDelete canShare
  where
    canView = True
    canCreate = True
    canDelete = True
    canShare = True

objRefWithNoOverviewPerms :: ObjectReference
objRefWithNoOverviewPerms = Capability.OverviewRef $ Capability.mkOverviewPerms canView canCreate canDelete canShare
  where
    canView = False
    canCreate = False
    canDelete = False
    canShare = False

objRefWithAllArticlesPerms :: ObjectReference
objRefWithAllArticlesPerms =
    Capability.ArticlesRef $
        Capability.mkArticlesPerms canView canCreate canChangeTitles canChangeStates canDelete canShare
  where
    canView = True
    canCreate = True
    canChangeTitles = True
    canChangeStates = True
    canDelete = True
    canShare = True

objRefWithNoArticlesPerms :: ObjectReference
objRefWithNoArticlesPerms =
    Capability.ArticlesRef $
        Capability.mkArticlesPerms canView canCreate canChangeTitles canChangeStates canDelete canShare
  where
    canView = False
    canCreate = False
    canChangeTitles = False
    canChangeStates = False
    canDelete = False
    canShare = False

objRefWithAllArticlePerms :: Id Article -> ObjectReference
objRefWithAllArticlePerms artId =
    Capability.ArticleRef artId $
        Capability.mkArticlePerms artId canView canChangeTitle canChangeState canDelete canShare
  where
    canView = True
    canChangeTitle = True
    canChangeState = True
    canDelete = True
    canShare = True

objRefWithNoArticlePerms :: Id Article -> ObjectReference
objRefWithNoArticlePerms artId =
    Capability.ArticleRef artId $
        Capability.mkArticlePerms artId canView canChangeTitle canChangeState canDelete canShare
  where
    canView = False
    canChangeTitle = False
    canChangeState = False
    canDelete = False
    canShare = False
