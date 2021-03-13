module Lib.Core.Action.Command
  ( deleteGetArticles,
    deleteArticle,
    changeArticleTitle,
    archiveArticle,
    unreadArticle,
    createArticle,
    createResource,
    createGetArticlesCap,
  )
where

import qualified Data.HashSet as Set
import qualified Data.Text as Text
import Lib.App.Error (WithError, missingParameter, serverError, throwError)
import Lib.Core.Domain.Accesstoken (Accesstoken, Reference (..), mkAccesstoken)
import Lib.Core.Domain.Article (Article (..), ArticleState (..))
import Lib.Core.Domain.Capability (Action (..), Capability (..), CommandAction (..), CreateGetArticlesCapActions (..), DeleteAction (..), FrontendAction (..), GetArticleActions (..), GetArticlesActions (..), PatchAction (..), PostAction (..), QueryAction (..), ResourceOverviewActions (..))
import Lib.Core.Domain.Context (Context (..))
import Lib.Core.Domain.Entity (Entity (..))
import qualified Lib.Core.Domain.Entity as Entity
import Lib.Core.Domain.ExpirationDate (ExpirationDate)
import Lib.Core.Domain.Id (Id)
import Lib.Core.Domain.StoreEvent (StoreEvent)
import Lib.Core.Domain.Uri (Uri)
import Lib.Core.Effect.Random (MonadRandom (..))
import Lib.Core.Effect.Repository (Persist (..), RWCapabilities, RWEntity)
import qualified Lib.Core.Effect.Repository as R
import Lib.Core.Effect.Scraper (MonadScraper (..))
import Lib.Core.Effect.Time (MonadTime (..))
import Lib.Web.Route.Common (collectionMainR, linkAsText, listArticlesR, showArticleR)
import Servant (Link)
import UnliftIO.Async (concurrently)

deleteGetArticles :: (Persist m) => Context -> Id Capability -> m ()
deleteGetArticles ctx gaCapId = do
  let resId = resourceId $ ctxRef ctx

  R.commit resId $
    -- Delete the stored capability id for the GetArticles action
    one (R.deleteCap gaCapId)
      -- Delete the capability and action for this DeleteGetArticles
      <> one (R.deleteCap (Entity.id $ ctxCap ctx))
      <> one (R.deleteAct (Entity.id $ ctxAct ctx))

deleteArticle :: (Persist m) => Context -> Id Article -> m ()
deleteArticle ctx =
  R.commit (resourceId $ ctxRef ctx) . one . R.deleteArt

changeArticleTitle ::
  (RWEntity Article m) => Context -> Id Article -> Maybe Text -> m ()
changeArticleTitle ctx artId mTitle = do
  let resId = resourceId $ ctxRef ctx
  -- Update title of the article when it was part of the request, and do nothing
  -- if it’s missing.
  case mTitle of
    Nothing -> pure ()
    Just title ->
      R.commit resId . one $ R.artUpdateTitle artId title

archiveArticle ::
  (RWEntity Article m) => Context -> Id Article -> m ()
archiveArticle ctx artId = do
  let resId = resourceId $ ctxRef ctx
  R.commit resId . one $ R.artUpdateState artId Archived

unreadArticle :: (RWEntity Article m) => Context -> Id Article -> m ()
unreadArticle ctx artId = do
  let resId = resourceId $ ctxRef ctx
  R.commit resId . one $ R.artUpdateState artId Unread

data ArticleAction a = ArticleAction
  { _aaCat :: !a,
    _aaAa :: !a,
    _aaUa :: !a,
    aaDa :: !a
  }
  deriving stock (Functor, Foldable, Traversable)

createArticle ::
  ( RWCapabilities m,
    MonadRandom m,
    MonadScraper m,
    MonadTime m,
    WithError m
  ) =>
  Context ->
  Id Action ->
  Maybe Uri ->
  m ()
createArticle ctx getArticlesId mUri = do
  let resId = resourceId $ ctxRef ctx
  let mExpDate = capExpirationDate . Entity.val $ ctxCap ctx
  case mUri of
    Nothing -> throwError $ missingParameter "The parameter `uri` is missing."
    Just uri -> do
      -- Create and store article
      (Entity articleId articleVal) <- createArticleEnt uri
      let storeArticleEvent = R.insertArt articleId articleVal

      -- Create and store actions and frontend actions for GetArticle
      seAndActEnts <- traverse insertAction $ createArticleActions articleId
      let actEnts = snd <$> seAndActEnts
      let articleActionsEvent = fromList . toList $ fst <$> seAndActEnts

      getArticleActId <- getRandomId

      seAndActFrontEnts <-
        traverse insertAction $
          createArticleFrontendActions
            (Entity.id <$> actEnts)
            getArticleActId
            showArticleR
      let actFrontEnts = snd <$> seAndActFrontEnts
      let articleFrontActionsEvent =
            fromList . toList $ fst <$> seAndActFrontEnts

      -- Create and store separate GetArticle action which is used during the
      -- GetArticles action.
      -- The GetArticles controller expects that the query is an GetArticles
      -- action.
      -- We also replace the DeleteArticle frontend action from actFrontEnts
      -- with this one, because redirecting to the deleted article’s page does
      -- not make sense.
      seAndActFrontEnts' <-
        traverse insertAction $
          createArticleFrontendActions
            (Entity.id <$> actEnts)
            getArticlesId
            listArticlesR
      let actFrontEnts' = snd <$> seAndActFrontEnts'
      let articleFrontActionsEvent' =
            fromList . toList $ fst <$> seAndActFrontEnts'

      getArticleActId' <- getRandomId
      let getArticleActionEvent' =
            R.insertAct getArticleActId' $
              createGetArticleAction articleId getArticleActId actFrontEnts'

      let upActFrontEnts = actFrontEnts {aaDa = aaDa actFrontEnts'}
      let getArticleActionEvent =
            R.insertAct getArticleActId $
              createGetArticleAction articleId getArticleActId upActFrontEnts

      -- Create and store all capabilities.
      capabilitiesEvent <-
        fmap fromList
          . traverse (fmap fst . insertCapability)
          . createArticleCapabilities mExpDate
          $ getArticleActId :
          getArticleActId' :
          concatMap
            (fmap Entity.id . toList)
            [actEnts, actFrontEnts, actFrontEnts']

      -- Update existing GetArticles action with new data.
      let getArticlesActionEvent = updateGetArticlesAction getArticleActId'

      -- Commit all StoreEvents
      R.commit resId $
        one storeArticleEvent
          <> articleActionsEvent
          <> articleFrontActionsEvent
          <> articleFrontActionsEvent'
          <> one getArticleActionEvent'
          <> one getArticleActionEvent
          <> capabilitiesEvent
          <> one getArticlesActionEvent
  where
    createArticleEnt ::
      (MonadRandom m, MonadScraper m, MonadTime m) => Uri -> m (Entity Article)
    createArticleEnt uri = do
      aId <- getRandomId
      (aTitle, aCreated) <- concurrently (scrapWebsite uri) getCurrentTime
      let aState = Unread
      pure . Entity aId $ Article aTitle uri aState aCreated

    createArticleActions :: Id Article -> ArticleAction Action
    createArticleActions artId =
      ArticleAction
        (Command . Patch $ ChangeArticleTitle artId)
        (Command . Patch $ ArchiveArticle artId)
        (Command . Patch $ UnreadArticle artId)
        (Command . Delete $ DeleteArticle artId)

    createGetArticleAction ::
      Id Article -> Id Action -> ArticleAction (Entity Action) -> Action
    createGetArticleAction artId getArtActId actEnts =
      let (ArticleAction cat aa ua da) = Just . Entity.id <$> actEnts
       in Query
            . GetArticle artId
            $ GetArticleActions getArtActId cat aa ua da (Just getArticlesId)

    createArticleFrontendActions ::
      ArticleAction (Id Action) ->
      Id Action ->
      (Maybe Accesstoken -> Link) ->
      ArticleAction Action
    createArticleFrontendActions commandIds queryId route =
      let makeFAct cId =
            Frontend
              . FrontendAction cId queryId
              . linkAsText
              $ route Nothing
       in fmap makeFAct commandIds

    createArticleCapabilities ::
      Maybe ExpirationDate -> [Id Action] -> [Capability]
    createArticleCapabilities mExpDate = fmap (Capability Nothing mExpDate)

    updateGetArticlesAction :: Id Action -> StoreEvent
    updateGetArticlesAction getArticleId =
      R.updateAct getArticlesId $ \oldActEnt -> case oldActEnt of
        Query qAction -> case qAction of
          GetArticles oldGetArticlesAct ->
            let oldGetArticlesSet = gaaShowArticles oldGetArticlesAct
                newGetArticlesSet = Set.insert getArticleId oldGetArticlesSet
                newGetArticlesAct = oldGetArticlesAct {gaaShowArticles = newGetArticlesSet}
             in Query $ GetArticles newGetArticlesAct
          _wrongAction -> oldActEnt
        _wrongAction -> oldActEnt

createResource :: (Persist m, MonadRandom m) => m Text
createResource = do
  -- Generate a new resource.
  resId <- getRandomId
  R.init resId

  -- Create actions that are always available and don’t depend on other data.
  (gagacaSe, gagacaEnt) <-
    insertAction . Query $ GetActiveGetArticlesCaps Set.empty

  -- Create GetArticles and give it the id of CreateArticle.
  caaActId <- getRandomId

  let gaActions =
        GetArticlesActions
          (Just caaActId)
          Nothing
          Set.empty
  (gaaSe, gaaEnt) <- insertAction . Query $ GetArticles gaActions

  -- Create CreateArticle and give it the GetArticles id.
  let caSe =
        R.insertAct caaActId . Command . Post . CreateArticle $ Entity.id gaaEnt

  -- Generate Resource Overview id here because we need it earlier.
  roaActId <- getRandomId

  -- Create CreateGetArticlesCap and give it the GetArticles id and
  -- GetActiveGetArticlesCaps id.
  let cgaa =
        Command
          . Post
          . CreateGetArticlesCap
          $ CreateGetArticlesCapActions
            (Entity.id gaaEnt)
            (Entity.id gagacaEnt)
            roaActId
  (cgaaSe, cgaaEnt) <- insertAction cgaa

  -- Create ResourceOverview and give it GetActiveGetArticlesCap, GetArticles,
  -- and CreateGetArticlesCap initially.
  let roaActions =
        ResourceOverviewActions
          (Entity.id gagacaEnt)
          (Entity.id gaaEnt)
          (Just $ Entity.id cgaaEnt)
          Nothing
  let roaAct = Query $ ResourceOverview roaActions
  let roaSe = R.insertAct roaActId roaAct
  let roaEnt = Entity roaActId roaAct

  -- Create capability for ResourceOverview, store it and create an
  -- accesstoken.
  let roaCap = Capability Nothing Nothing $ Entity.id roaEnt
  (roaCapSe, roaCapEnt) <- insertCapability roaCap
  let acc = mkAccesstoken . Reference resId $ Entity.id roaCapEnt

  -- Create FrontendAction for GetArticles and ResourceOverview and store them.
  (cafaSe, _cafaEnt) <- createFga caaActId (Entity.id gaaEnt)
  (cgafaSe, cgafaEnt) <-
    createFro acc (Entity.id cgaaEnt) (Entity.id roaEnt)

  -- Create capabilitiy for CreateGetArticlesCap and store it.
  (cgacSe, _cgacEnt) <-
    insertCapability
      . Capability Nothing Nothing
      . Entity.id
      $ cgafaEnt

  R.commit resId $
    one gagacaSe
      <> one gaaSe
      <> one caSe
      <> one cgaaSe
      <> one roaSe
      <> one roaCapSe
      <> cafaSe
      <> cgafaSe
      <> one cgacSe

  -- Return the ResourceOverview accesstoken.
  pure . linkAsText . collectionMainR $ Just acc
  where
    createFga caaId gaaId = do
      let cafaRoute = linkAsText $ listArticlesR Nothing
      let cafa = Frontend $ FrontendAction caaId gaaId cafaRoute
      (cafaSe, cafaEnt) <- insertAction cafa

      let updateSe =
            R.updateAct gaaId $ \oldAct -> case oldAct of
              Query qAction -> case qAction of
                GetArticles oldGaActs ->
                  Query
                    . GetArticles
                    $ oldGaActs {gaaFrontCreateArticle = Just $ Entity.id cafaEnt}
                _wrongAction -> oldAct
              _wrongAction -> oldAct

      pure (one cafaSe <> one updateSe, cafaEnt)

    createFro acc cgaaId roaId = do
      let cgafRoute = linkAsText . collectionMainR $ Just acc
      let cgafa = Frontend $ FrontendAction cgaaId roaId cgafRoute
      (cgafaSe, cgafaEnt) <- insertAction cgafa

      let updateSe =
            R.updateAct roaId $ \oldAct -> case oldAct of
              Query qAction -> case qAction of
                ResourceOverview oldRoaActions ->
                  Query
                    . ResourceOverview
                    $ oldRoaActions {roaFrontCreateGetArticlesCap = Just $ Entity.id cgafaEnt}
                _wrongAction -> oldAct
              _wrongAction -> oldAct

      pure (one cgafaSe <> one updateSe, cgafaEnt)

createGetArticlesCap ::
  (RWCapabilities m, MonadRandom m, WithError m) =>
  Context ->
  Maybe Text ->
  Maybe ExpirationDate ->
  CreateGetArticlesCapActions ->
  m ()
createGetArticlesCap ctx mUnlockPetname mExpDate CreateGetArticlesCapActions {..} = do
  let resId = resourceId $ ctxRef ctx
  gaActEnt <- R.getOneAct resId cgacGetArticles
  (GetArticlesActions mId1 mId2 showArticleIds) <-
    getGaActions $ Entity.val gaActEnt
  let saIds = Just <$> Set.toList showArticleIds
  let gaActions = catMaybes $ [mId1, mId2] ++ saIds

  let unlockPetname = convertPetname mUnlockPetname
  -- Create capabilities for all action ids in GetArticles and for GetArticles.
  gaSe <-
    fromList
      <$> traverse
        (fmap fst . insertCapability . Capability Nothing mExpDate)
        gaActions

  (gaCapSe, gaCapEnt) <-
    insertCapability . Capability unlockPetname mExpDate $ Entity.id gaActEnt

  -- Create action and capability for DeleteGetArticles.
  let dga = Command . Delete . DeleteGetArticles $ Entity.id gaCapEnt
  (dgaSe, dgaEnt) <- insertAction dga
  let dgaCap = Capability Nothing mExpDate $ Entity.id dgaEnt
  (dgaCapSe, _dgaCapEnt) <- insertCapability dgaCap

  roaAcc <-
    mkAccesstoken
      . Reference resId
      <$> R.getCapIdForActId resId cgacResourceOverview

  let dgaF =
        Frontend
          . FrontendAction (Entity.id dgaEnt) cgacResourceOverview
          . linkAsText
          $ collectionMainR (Just roaAcc)
  (dgaFSe, dgaFEnt) <- insertAction dgaF
  let dgaFCap = Capability Nothing mExpDate $ Entity.id dgaFEnt
  (dgaFCapSe, dgaFCapEnt) <- insertCapability dgaFCap

  -- Lookup 'GetActiveGetArticlesCaps' and update the 'Set' of revocable
  -- links to the 'GetArticles' 'Action'.
  let cgacSe = R.updateAct cgacGetActiveGetArticlesCap $ \oldAct ->
        case oldAct of
          Query qAction -> case qAction of
            GetActiveGetArticlesCaps gagaSet ->
              Query
                . GetActiveGetArticlesCaps
                $ Set.insert (Entity.id gaCapEnt, Entity.id dgaFCapEnt) gagaSet
            _wrongAction -> oldAct
          _wrongAction -> oldAct

  R.commit resId $
    gaSe
      <> one gaCapSe
      <> one dgaSe
      <> one dgaCapSe
      <> one dgaFSe
      <> one dgaFCapSe
      <> one cgacSe
  where
    getGaActions :: (WithError m) => Action -> m GetArticlesActions
    getGaActions = \case
      Query qAction -> case qAction of
        GetArticles gaActions -> pure gaActions
        _wrongAction -> wrongAction
      _wrongAction -> wrongAction

    convertPetname :: Maybe Text -> Maybe Text
    convertPetname = \case
      Nothing -> Nothing
      Just petname -> if Text.null petname then Nothing else Just petname

    wrongAction :: (WithError m) => m a
    wrongAction = throwError $ serverError "Wrong action called"

insertAction ::
  (MonadRandom m) => Action -> m (StoreEvent, Entity Action)
insertAction act = do
  actId <- getRandomId
  pure (R.insertAct actId act, Entity actId act)

insertCapability ::
  (MonadRandom m) => Capability -> m (StoreEvent, Entity Capability)
insertCapability cap = do
  capId <- getRandomId
  pure (R.insertCap capId cap, Entity capId cap)
