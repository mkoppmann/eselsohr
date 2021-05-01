module Lib.Core.Action.Command
  ( deleteGetArticles
  , deleteArticle
  , changeArticleTitle
  , archiveArticle
  , unreadArticle
  , createArticle
  , createResource
  , createGetArticlesCap
  ) where

import qualified Data.HashSet                  as Set
import qualified Data.Text                     as Text
import           Lib.App.Error                  ( WithError
                                                , missingParameter
                                                , serverError
                                                , throwError
                                                )
import           Lib.Core.Domain.Accesstoken    ( Reference(..)
                                                , mkAccesstoken
                                                )
import           Lib.Core.Domain.Article        ( Article(..)
                                                , ArticleState(..)
                                                )
import           Lib.Core.Domain.Capability     ( Action(..)
                                                , Capability(..)
                                                , CommandAction(..)
                                                , CreateGetArticlesCapActions(..)
                                                , DeleteAction(..)
                                                , GetArticleActions(..)
                                                , GetArticlesActions(..)
                                                , PatchAction(..)
                                                , PostAction(..)
                                                , QueryAction(..)
                                                , ResourceOverviewActions(..)
                                                )
import           Lib.Core.Domain.Context        ( Context(..) )
import           Lib.Core.Domain.Entity         ( Entity(..) )
import qualified Lib.Core.Domain.Entity        as Entity
import           Lib.Core.Domain.ExpirationDate ( ExpirationDate )
import           Lib.Core.Domain.Id             ( Id )
import           Lib.Core.Domain.StoreEvent     ( StoreEvent )
import           Lib.Core.Domain.Uri            ( Uri )
import           Lib.Core.Effect.Random         ( MonadRandom(..) )
import           Lib.Core.Effect.Repository     ( ContextState(..)
                                                , WriteState(..)
                                                )
import qualified Lib.Core.Effect.Repository    as R
import           Lib.Core.Effect.Scraper        ( MonadScraper(..) )
import           Lib.Core.Effect.Time           ( MonadTime(..) )
import           Lib.Web.Route.Common           ( collectionMainR
                                                , linkAsText
                                                )

deleteGetArticles :: (WriteState m) => ContextState -> Id Capability -> m ()
deleteGetArticles ctx gaCapId = do
  let resId  = resourceId . ctxRef $ csContext ctx
      capEnt = ctxCap $ csContext ctx
      actEnt = ctxAct $ csContext ctx

  R.commit resId
    $  one (R.deleteCap gaCapId)
    <> one (R.deleteCap (Entity.id capEnt))
    <> one (R.deleteAct (Entity.id actEnt))

deleteArticle :: (WriteState m) => ContextState -> Id Article -> m ()
deleteArticle ctx artId = do
  let resId = resourceId . ctxRef $ csContext ctx
  R.commit resId . one $ R.deleteArt artId

changeArticleTitle
  :: (WriteState m) => ContextState -> Id Article -> Maybe Text -> m ()
changeArticleTitle ctx artId mTitle = do
  let resId = resourceId . ctxRef $ csContext ctx
  case mTitle of
    Nothing    -> pass
    Just title -> R.commit resId . one $ R.artUpdateTitle artId title

archiveArticle :: (WriteState m) => ContextState -> Id Article -> m ()
archiveArticle ctx artId = do
  let resId = resourceId . ctxRef $ csContext ctx
  R.commit resId . one $ R.artUpdateState artId Archived

unreadArticle :: (WriteState m) => ContextState -> Id Article -> m ()
unreadArticle ctx artId = do
  let resId = resourceId . ctxRef $ csContext ctx
  R.commit resId . one $ R.artUpdateState artId Unread

data ArticleAction a = ArticleAction
  { _aaCat :: !a
  , _aaAa  :: !a
  , _aaUa  :: !a
  , _aaDa  :: !a
  }
  deriving stock (Functor, Foldable, Traversable)

createArticle
  :: (WriteState m, MonadRandom m, MonadScraper m, MonadTime m, WithError m)
  => ContextState
  -> Id Action
  -> Maybe Uri
  -> m ()
createArticle ctx getArticlesId mUri = do
  let resId    = resourceId . ctxRef $ csContext ctx
      mExpDate = capExpirationDate . Entity.val . ctxCap $ csContext ctx
  case mUri of
    Nothing  -> throwError $ missingParameter "The parameter `uri` is missing."
    Just uri -> do
      (Entity articleId articleVal) <- createArticleEnt uri
      let storeArticleEvent = R.insertArt articleId articleVal

      seAndActEnts <- traverse insertAction $ createArticleActions articleId
      let actEnts             = snd <$> seAndActEnts
          articleActionsEvent = fromList . toList $ fst <$> seAndActEnts

      getArticleActId <- getRandomId
      let getArticleActionEvent =
            R.insertAct getArticleActId
              $ createGetArticleAction articleId getArticleActId actEnts

      capabilitiesEvent <-
        fmap fromList
        . traverse (fmap fst . insertCapability)
        . createArticleCapabilities mExpDate
        $ getArticleActId
        : (Entity.id <$> toList actEnts)

      let getArticlesActionEvent = updateGetArticlesAction getArticleActId

      R.commit resId
        $  one storeArticleEvent
        <> articleActionsEvent
        <> one getArticleActionEvent
        <> capabilitiesEvent
        <> one getArticlesActionEvent
 where
  createArticleEnt
    :: (MonadRandom m, MonadScraper m, MonadTime m) => Uri -> m (Entity Article)
  createArticleEnt uri = do
    aId      <- getRandomId
    aTitle   <- scrapWebsite uri
    aCreated <- getCurrentTime
    let aState = Unread
    pure . Entity aId $ Article aTitle uri aState aCreated

  createArticleActions :: Id Article -> ArticleAction Action
  createArticleActions artId = ArticleAction
    (Command . Patch $ ChangeArticleTitle artId)
    (Command . Patch $ ArchiveArticle artId)
    (Command . Patch $ UnreadArticle artId)
    (Command . Delete $ DeleteArticle artId)

  createGetArticleAction
    :: Id Article -> Id Action -> ArticleAction (Entity Action) -> Action
  createGetArticleAction artId getArtActId actEnts = do
    let (ArticleAction cat aa ua da) = Just . Entity.id <$> actEnts
    Query . GetArticle artId $ GetArticleActions getArtActId
                                                 cat
                                                 aa
                                                 ua
                                                 da
                                                 (Just getArticlesId)

  createArticleCapabilities
    :: Maybe ExpirationDate -> [Id Action] -> [Capability]
  createArticleCapabilities mExpDate = fmap (Capability Nothing mExpDate)

  updateGetArticlesAction :: Id Action -> StoreEvent
  updateGetArticlesAction getArticleId =
    R.updateAct getArticlesId $ \oldActEnt -> case oldActEnt of
      Query qAction -> case qAction of
        GetArticles oldGetArticlesAct ->
          let
            oldGetArticlesSet = gaaShowArticles oldGetArticlesAct
            newGetArticlesSet = Set.insert getArticleId oldGetArticlesSet
            newGetArticlesAct =
              oldGetArticlesAct { gaaShowArticles = newGetArticlesSet }
          in
            Query $ GetArticles newGetArticlesAct
        _wrongAction -> oldActEnt
      _wrongAction -> oldActEnt

createResource :: (WriteState m, MonadRandom m) => m Text
createResource = do
  resId <- getRandomId
  R.init resId

  (gagacaSe, gagacaEnt) <- insertAction . Query $ GetActiveGetArticlesCaps
    Set.empty

  caaActId <- getRandomId
  let gaActions = GetArticlesActions (Just caaActId) Set.empty
  (gaaSe, gaaEnt) <- insertAction . Query $ GetArticles gaActions

  let caSe =
        R.insertAct caaActId . Command . Post . CreateArticle $ Entity.id gaaEnt

  roaActId <- getRandomId
  let cgaa =
        Command . Post . CreateGetArticlesCap $ CreateGetArticlesCapActions
          (Entity.id gaaEnt)
          (Entity.id gagacaEnt)
          roaActId
  (cgaaSe, cgaaEnt) <- insertAction cgaa

  let roaActions = ResourceOverviewActions (Entity.id gagacaEnt)
                                           (Entity.id gaaEnt)
                                           (Just $ Entity.id cgaaEnt)
      roaAct = Query $ ResourceOverview roaActions
      roaSe  = R.insertAct roaActId roaAct
      roaEnt = Entity roaActId roaAct

  let roaCap = Capability Nothing Nothing $ Entity.id roaEnt
  (roaCapSe, roaCapEnt) <- insertCapability roaCap
  let acc = mkAccesstoken . Reference resId $ Entity.id roaCapEnt

  (cgacSe, _cgacEnt) <-
    insertCapability . Capability Nothing Nothing . Entity.id $ cgaaEnt

  R.commit resId
    $  one gagacaSe
    <> one gaaSe
    <> one caSe
    <> one cgaaSe
    <> one roaSe
    <> one roaCapSe
    <> one cgacSe

  pure . linkAsText . collectionMainR $ Just acc

createGetArticlesCap
  :: (WriteState m, MonadRandom m, WithError m)
  => ContextState
  -> Maybe Text
  -> Maybe ExpirationDate
  -> CreateGetArticlesCapActions
  -> m ()
createGetArticlesCap ctx mUnlockPetname mExpDate CreateGetArticlesCapActions {..}
  = do
    let resId = resourceId . ctxRef $ csContext ctx
        res   = csResource ctx

    gaActEnt <- R.getOneAct res cgacGetArticles
    (GetArticlesActions gaaCreateArticle showArticleIds) <- getGaActions
      $ Entity.val gaActEnt
    let saIds     = Just <$> Set.toList showArticleIds
        gaActions = catMaybes $ gaaCreateArticle : saIds

    let unlockPetname = convertPetname mUnlockPetname
    gaSe <-
      fromList
        <$> traverse
              (fmap fst . insertCapability . Capability Nothing mExpDate)
              gaActions

    (gaCapSe, gaCapEnt) <-
      insertCapability . Capability unlockPetname mExpDate $ Entity.id gaActEnt

    let dga = Command . Delete . DeleteGetArticles $ Entity.id gaCapEnt
    (dgaSe, dgaEnt) <- insertAction dga
    let dgaCap = Capability Nothing mExpDate $ Entity.id dgaEnt
    (dgaCapSe, dgaCapEnt) <- insertCapability dgaCap

    let cgacSe = updateCreateGetArticlesCap gaCapEnt dgaCapEnt

    R.commit resId
      $  gaSe
      <> one gaCapSe
      <> one dgaSe
      <> one dgaCapSe
      <> one cgacSe
 where
  getGaActions :: (WithError m) => Action -> m GetArticlesActions
  getGaActions (Query qAction) = case qAction of
    GetArticles gaActions -> pure gaActions
    _wrongAction          -> wrongAction
  getGaActions _wrongAction = wrongAction

  convertPetname :: Maybe Text -> Maybe Text
  convertPetname Nothing      = Nothing
  convertPetname (Just pName) = if Text.null pName then Nothing else Just pName

  updateCreateGetArticlesCap
    :: Entity Capability -> Entity Capability -> StoreEvent
  updateCreateGetArticlesCap gaCapEnt dgaCapEnt = R.updateAct
    cgacGetActiveGetArticlesCap
    updateFunc
   where
    updateFunc oldAct@(Query qAction) = case qAction of
      GetActiveGetArticlesCaps gagaSet ->
        Query . GetActiveGetArticlesCaps $ Set.insert
          (Entity.id gaCapEnt, Entity.id dgaCapEnt)
          gagaSet
      _wrongAction -> oldAct
    updateFunc oldAct@_wrongAction = oldAct

  wrongAction :: (WithError m) => m a
  wrongAction = throwError $ serverError "Wrong action called"

insertAction :: (MonadRandom m) => Action -> m (StoreEvent, Entity Action)
insertAction act = do
  actId <- getRandomId
  pure (R.insertAct actId act, Entity actId act)

insertCapability
  :: (MonadRandom m) => Capability -> m (StoreEvent, Entity Capability)
insertCapability cap = do
  capId <- getRandomId
  pure (R.insertCap capId cap, Entity capId cap)
