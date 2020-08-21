module Lib.Web.Handler.Common
  ( listArticlesR,
    showArticleR,
    editArticleR,
    stylesheetR,
    actionR,
    linkAsText,
    getAccIdForAction,
    addCapAndAccesstoken,
  )
where

import Lib.App.Error (WithError, dbError, throwError)
import Lib.Core.Accesstoken (Accesstoken (..))
import Lib.Core.Capability (Capability (..))
import Lib.Core.Collection (Collection)
import Lib.Core.Id (AnyId, Id)
import Lib.Core.UserAction (UserAction (..))
import Lib.Effect.Random (MonadRandom (..))
import Lib.Effect.Resource (CommandAccesstoken (..), CommandCollection (..), QueryAccesstoken (..), QueryCollection (..))
import qualified Lib.Web.Route as Route
import Servant (Link, fieldLink, linkURI)

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

getAccesstokenForAction ::
  ( QueryAccesstoken m,
    QueryCollection m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m [Accesstoken]
getAccesstokenForAction collId mId userAction =
  maybe tokensWithoutEntity tokensWithEntity mId
  where
    getAccesstokens capIds =
      traverse getAccesstokenViaCapability capIds

    tokensWithEntity aId = do
      caps <- getAllCapabilitiesViaEntity collId aId
      getAccesstokens $ capabilityId <$> filter (\cap -> capabilityAction cap == userAction) caps

    tokensWithoutEntity = do
      cap <- getCapabilityViaAction collId userAction
      getAccesstokens . one $ capabilityId cap

getAccIdForAction ::
  ( QueryAccesstoken m,
    QueryCollection m,
    WithError m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m (Id Accesstoken)
getAccIdForAction collId mAId userAction = do
  tokens <- getAccesstokenForAction collId mAId userAction
  case viaNonEmpty head tokens of
    Nothing ->
      throwError . dbError $
        "Error getting accesstoken for action: " <> show userAction
    Just token -> return $ accesstokenId token

addCapAndAccesstoken ::
  ( CommandAccesstoken m,
    CommandCollection m,
    MonadRandom m
  ) =>
  Id Collection ->
  Maybe AnyId ->
  UserAction ->
  m (Id Accesstoken)
addCapAndAccesstoken collId entityId userAction = do
  capId <- getRandomId
  accId <- getRandomId
  let cap = Capability capId entityId userAction
  let acc = Accesstoken accId collId capId
  insertCapability collId cap
  insertAccesstoken acc
  pure accId
