module Lib.Web.Handler.Common
  ( -- * Route helpers
    collectionMainR,
    collectionSettingsR,
    collectionShareR,
    listArticlesR,
    showArticleR,
    editArticleR,
    stylesheetR,
    actionR,
    linkAsText,

    -- * Resource helpers
    initAccDb,
    initCollDb,
    addCap,
    addAcc,
    addCapAndAcc,
    getAccIdForAction,
    getAccIdsForAction,
    getAccIdViaCapId,
    getAccIdsViaCapId,
    getCollIdAndActionViaAccId,
  )
where

import Lib.App.Error (WithError)
import Lib.Core.Accesstoken (Accesstoken (..))
import Lib.Core.Article (Article)
import Lib.Core.Capability (Capability (..))
import Lib.Core.Collection (Collection)
import Lib.Core.Id (AnyId, Id)
import Lib.Core.UserAction (UserAction (..))
import Lib.Effect.Log (WithLog, log, pattern D)
import Lib.Effect.Random (MonadRandom (..))
import qualified Lib.Effect.Resource as Res
import qualified Lib.Web.Route as Route
import Servant (Link, fieldLink, linkURI)

-- Route helpers

collectionMainR :: Maybe (Id Accesstoken) -> Link
collectionMainR = fieldLink Route.collectionMain

collectionSettingsR :: Maybe (Id Accesstoken) -> Link
collectionSettingsR = fieldLink Route.collectionMain

collectionShareR :: Maybe (Id Accesstoken) -> Link
collectionShareR = fieldLink Route.collectionMain

listArticlesR :: Maybe (Id Accesstoken) -> Link
listArticlesR = fieldLink Route.listArticles

showArticleR :: Maybe (Id Accesstoken) -> Link
showArticleR = fieldLink Route.showArticle

editArticleR :: Maybe (Id Accesstoken) -> Link
editArticleR = fieldLink Route.editArticle

stylesheetR :: Link
stylesheetR = fieldLink Route.stylesheet

actionR :: Link
actionR = fieldLink Route.postAction

linkAsText :: Link -> LText
linkAsText = (<>) "/" . show @LText . linkURI

initAccDb ::
  ( Res.CommandEntity Accesstoken m,
    Res.CommandEntity Capability m
  ) =>
  Id Collection ->
  m ()
initAccDb cId = initAccTable >> initCapTable
  where
    initAccTable = Res.init @Accesstoken cId
    initCapTable = Res.init @Capability cId

initCollDb ::
  ( Res.CommandEntity Article m,
    Res.CommandEntity Capability m
  ) =>
  Id Collection ->
  m ()
initCollDb cId = initArtTable >> initCapTable
  where
    initArtTable = Res.init @Article cId
    initCapTable = Res.init @Capability cId

addCap ::
  ( Res.CommandEntity Capability m,
    MonadRandom m,
    WithLog env m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m (Id Capability)
addCap cId entId uAction = do
  capId <- getRandomId
  log D $ "Adding capability for action: " <> show uAction
  Res.insert cId $ Capability capId entId uAction
  log D $ "Capability added with id: " <> show capId
  pure capId

addAcc ::
  ( Res.CommandEntity Accesstoken m,
    MonadRandom m,
    WithLog env m
  ) =>
  Id Collection ->
  Id Capability ->
  m (Id Accesstoken)
addAcc cId capId = do
  accId <- getRandomId
  log D $ "Adding accesstoken for capId: " <> show capId
  Res.insert Res.accDbId $ Accesstoken accId cId capId
  log D "Accesstoken added"
  pure accId

addCapAndAcc ::
  ( Res.CommandEntity Accesstoken m,
    Res.CommandEntity Capability m,
    MonadRandom m,
    WithLog env m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m (Id Accesstoken)
addCapAndAcc cId entId = addAcc cId <=< addCap cId entId

getAccIdForAction ::
  ( Res.QueryEntity Accesstoken m,
    Res.QueryEntity Capability m,
    WithError m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m (Id Accesstoken)
getAccIdForAction cId mEntId =
  pure . accesstokenId <=< Res.accGetOneViaAction cId mEntId

getAccIdsForAction ::
  ( Res.QueryEntity Accesstoken m,
    Res.QueryEntity Capability m,
    WithError m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m [Id Accesstoken]
getAccIdsForAction cId mEntId =
  pure . fmap accesstokenId <=< Res.accGetManyViaAction cId mEntId

getAccIdViaCapId ::
  ( Res.QueryEntity Accesstoken m,
    WithError m
  ) =>
  Id Collection ->
  Id Capability ->
  m (Id Accesstoken)
getAccIdViaCapId cId =
  pure . accesstokenId <=< Res.accGetOneViaCap cId

getAccIdsViaCapId ::
  ( Res.QueryEntity Accesstoken m
  ) =>
  Id Collection ->
  Id Capability ->
  m [Id Accesstoken]
getAccIdsViaCapId cId =
  pure . fmap accesstokenId <=< Res.accGetManyViaCap cId

getCollIdAndActionViaAccId ::
  ( Res.QueryEntity Accesstoken m,
    Res.QueryEntity Capability m
  ) =>
  Id Collection ->
  Id Accesstoken ->
  m (Id Collection, UserAction)
getCollIdAndActionViaAccId cId accId = do
  acc <- Res.getOne cId accId
  let collId = accesstokenCol acc
  let capId = accesstokenCap acc
  cap <- Res.getOne collId capId
  pure . (,) collId $ capabilityAction cap
