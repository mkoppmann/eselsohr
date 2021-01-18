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

import qualified Data.Set as Set
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
import Lib.Core.Domain.Resource (Resource)
import Lib.Core.Domain.Uri (Uri)
import Lib.Core.Effect.Random (MonadRandom (..))
import Lib.Core.Effect.Repository (RWCapabilities, RWEntity, WriteCapabilities (..), WriteEntity (..))
import qualified Lib.Core.Effect.Repository as R
import Lib.Core.Effect.Scraper (MonadScraper (..))
import Lib.Core.Effect.Time (MonadTime (..))
import Lib.Web.Route.Common (collectionMainR, linkAsText, listArticlesR, showArticleR)
import Servant (Link)
import UnliftIO.Async (concurrently)

deleteGetArticles :: (WriteCapabilities m) => Context -> Id Capability -> m ()
deleteGetArticles ctx gaCapId = do
  let resId = resourceId $ ctxRef ctx

  -- Delete the stored capability id for the GetArticles action
  R.deleteCap resId gaCapId

  -- Delete the capability and action for this DeleteGetArticles
  R.deleteCap resId . Entity.id $ ctxCap ctx
  R.deleteAct resId . Entity.id $ ctxAct ctx

deleteArticle :: (WriteEntity Article m) => Context -> Id Article -> m ()
deleteArticle ctx = R.deleteEnt (resourceId $ ctxRef ctx)

changeArticleTitle ::
  (RWEntity Article m) => Context -> Id Article -> Maybe Text -> m ()
changeArticleTitle ctx artId =
  -- Update title of the article when it was part of the request, and do nothing
  -- if it’s missing.
  maybe (pure ()) (R.artUpdateTitle (resourceId $ ctxRef ctx) artId)

archiveArticle ::
  (RWEntity Article m) => Context -> Id Article -> m ()
archiveArticle ctx artId =
  R.artUpdateState (resourceId $ ctxRef ctx) artId Archived

unreadArticle :: (RWEntity Article m) => Context -> Id Article -> m ()
unreadArticle ctx artId =
  R.artUpdateState (resourceId $ ctxRef ctx) artId Unread

data ArticleAction a = ArticleAction
  { _aaCat :: !a,
    _aaAa :: !a,
    _aaUa :: !a,
    aaDa :: !a
  }
  deriving stock (Functor, Foldable, Traversable)

createArticle ::
  ( RWCapabilities m,
    WriteEntity Article m,
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
      articleEnt <- createArticleEnt uri
      storeArticle resId articleEnt

      -- Create and store actions and frontend actions for GetArticle
      actEnts <-
        traverse (insertAction resId)
          . createArticleActions
          $ Entity.id articleEnt
      getArticleActId <- getRandomId
      R.insertAct resId getArticleActId
        =<< createGetArticleAction (Entity.id articleEnt) getArticleActId actEnts
      actFrontEnts <-
        traverse (insertAction resId) $
          createArticleFrontendActions
            (Entity.id <$> actEnts)
            getArticleActId
            showArticleR
      R.updateAct resId getArticleActId
        =<< createGetArticleAction (Entity.id articleEnt) getArticleActId actFrontEnts

      -- Create and store separate GetArticle action which is used during the
      -- GetArticles action.
      -- The GetArticles controller expects that the query is an GetArticles
      -- action.
      -- We also replace the DeleteArticle frontend action from actFrontEnts
      -- with this one, because redirecting to the deleted article’s page does
      -- not make sense.
      actFrontEnts' <-
        traverse (insertAction resId) $
          createArticleFrontendActions
            (Entity.id <$> actEnts)
            getArticlesId
            listArticlesR
      getArticleActId' <- getRandomId
      R.insertAct resId getArticleActId'
        =<< createGetArticleAction (Entity.id articleEnt) getArticleActId actFrontEnts'

      let upActFrontEnts = actFrontEnts {aaDa = aaDa actFrontEnts'}
      R.updateAct resId getArticleActId
        =<< createGetArticleAction (Entity.id articleEnt) getArticleActId upActFrontEnts

      -- Create and store all capabilities.
      _capEnts <-
        traverse (insertCapability resId)
          . createArticleCapabilities mExpDate
          $ getArticleActId :
          getArticleActId' :
          concatMap
            (map Entity.id . toList)
            [actEnts, actFrontEnts, actFrontEnts']

      -- Update existing GetArticles action with new data.
      updateGetArticlesAction resId getArticleActId'
  where
    createArticleEnt ::
      (MonadRandom m, MonadScraper m, MonadTime m) => Uri -> m (Entity Article)
    createArticleEnt uri = do
      aId <- getRandomId
      (aTitle, aCreated) <- concurrently (scrapWebsite uri) getCurrentTime
      let aState = Unread
      pure . Entity aId $ Article aTitle uri aState aCreated

    storeArticle ::
      (WriteEntity Article m) => Id Resource -> Entity Article -> m ()
    storeArticle resId (Entity artId artVal) = R.insertEnt resId artId artVal

    createArticleActions :: Id Article -> ArticleAction Action
    createArticleActions artId =
      ArticleAction
        (Command . Patch $ ChangeArticleTitle artId)
        (Command . Patch $ ArchiveArticle artId)
        (Command . Patch $ UnreadArticle artId)
        (Command . Delete $ DeleteArticle artId)

    createGetArticleAction ::
      (WithError m) =>
      Id Article ->
      Id Action ->
      ArticleAction (Entity Action) ->
      m Action
    createGetArticleAction artId getArtActId actEnts =
      let (ArticleAction cat aa ua da) = Just . Entity.id <$> actEnts
       in pure
            . Query
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

    createArticleCapabilities :: Maybe ExpirationDate -> [Id Action] -> [Capability]
    createArticleCapabilities mExpDate = fmap (Capability Nothing mExpDate)

    updateGetArticlesAction ::
      (RWCapabilities m, WithError m) => Id Resource -> Id Action -> m ()
    updateGetArticlesAction resId getArticleId = do
      actEnt <- R.getOneAct resId getArticlesId
      case Entity.val actEnt of
        Query qAction -> case qAction of
          GetArticles oldGetArticlesAct -> do
            let oldGetArticlesSet = gaaShowArticles oldGetArticlesAct
            let newGetArticlesSet = Set.insert getArticleId oldGetArticlesSet
            let newGetArticlesAct = oldGetArticlesAct {gaaShowArticles = newGetArticlesSet}
            let newGetArticles = Query $ GetArticles newGetArticlesAct
            R.updateAct resId getArticlesId newGetArticles
          _wrongQuery ->
            throwError $ serverError "Could not add new article to GetArticles"
        _nonQuery ->
          throwError $ serverError "Could not add new article to GetArticles"

createResource :: (WriteEntity Article m, MonadRandom m) => Context -> m Text
createResource _ = do
  -- Generate a new resource.
  resId <- getRandomId
  R.initRes resId (Nothing :: Maybe Article)

  -- Create actions that are always available and don’t depend on other data.
  gagacaEnt <- insertAction resId . Query $ GetActiveGetArticlesCaps Set.empty

  -- Create GetArticles and give it the id of CreateArticle.
  caaActId <- getRandomId

  let gaActions =
        GetArticlesActions
          (Just caaActId)
          Nothing
          Set.empty
  gaaEnt <- insertAction resId . Query $ GetArticles gaActions

  -- Create CreateArticle and give it the GetArticles id.
  R.insertAct resId caaActId . Command . Post . CreateArticle $ Entity.id gaaEnt

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
  cgaaEnt <- insertAction resId cgaa

  -- Create ResourceOverview and give it GetActiveGetArticlesCap, GetArticles,
  -- and CreateGetArticlesCap initially.
  let roaActions =
        ResourceOverviewActions
          (Entity.id gagacaEnt)
          (Entity.id gaaEnt)
          (Just $ Entity.id cgaaEnt)
          Nothing
  let roaAct = Query $ ResourceOverview roaActions
  R.insertAct resId roaActId roaAct
  let roaEnt = Entity roaActId roaAct

  -- Create FrontendAction for GetArticles and store it
  void $ createFga resId caaActId (Entity.id gaaEnt) gaActions

  -- Create capability for ResourceOverview, store it and create an
  -- accesstoken.
  let roaCap = Capability Nothing Nothing $ Entity.id roaEnt
  roaCapEnt <- insertCapability resId roaCap
  let acc = mkAccesstoken . Reference resId $ Entity.id roaCapEnt

  -- Create FrontendAction for ResourceOverview and store
  cgafaEnt <-
    createFro resId acc (Entity.id cgaaEnt) (Entity.id roaEnt) roaActions

  -- Create capabilitiy for CreateGetArticlesCap and store it.
  void
    . insertCapability resId
    . Capability Nothing Nothing
    . Entity.id
    $ cgafaEnt

  -- Return the ResourceOverview accesstoken.
  pure . linkAsText . collectionMainR $ Just acc
  where
    createFga resId caaId gaaId gaActs = do
      let cafaRoute = linkAsText $ listArticlesR Nothing
      let cafa = Frontend $ FrontendAction caaId gaaId cafaRoute
      cafaEnt <- insertAction resId cafa
      let newGaActions = gaActs {gaaFrontCreateArticle = Just $ Entity.id cafaEnt}
      let newGaa = Query $ GetArticles newGaActions
      R.updateAct resId gaaId newGaa
      pure cafaEnt

    createFro resId acc cgaaId roaId roaActs = do
      let cgafRoute = linkAsText . collectionMainR $ Just acc
      let cgafa = Frontend $ FrontendAction cgaaId roaId cgafRoute
      cgafaEnt <- insertAction resId cgafa
      let newRoaActions = roaActs {roaFrontCreateGetArticlesCap = Just $ Entity.id cgafaEnt}
      let newRoa = Query $ ResourceOverview newRoaActions
      R.updateAct resId roaId newRoa
      pure cgafaEnt

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
  traverse_ (insertCapability resId . Capability Nothing mExpDate) gaActions
  gaCapEnt <-
    insertCapability resId . Capability unlockPetname mExpDate $ Entity.id gaActEnt

  -- Create action and capability for DeleteGetArticles.
  let dga = Command . Delete . DeleteGetArticles $ Entity.id gaCapEnt
  dgaEnt <- insertAction resId dga
  let dgaCap = Capability Nothing mExpDate $ Entity.id dgaEnt
  void $ insertCapability resId dgaCap
  roaAcc <-
    mkAccesstoken
      . Reference resId
      <$> R.getCapIdForActId resId cgacResourceOverview
  let dgaF =
        Frontend
          . FrontendAction (Entity.id dgaEnt) cgacResourceOverview
          . linkAsText
          $ collectionMainR (Just roaAcc)
  dgaFEnt <- insertAction resId dgaF
  let dgaFCap = Capability Nothing mExpDate $ Entity.id dgaFEnt
  dgaFCapEnt <- insertCapability resId dgaFCap

  -- Lookup 'GetActiveGetArticlesCaps' and update the 'Set' of revocable
  -- links to the 'GetArticles' 'Action'.
  gagaa <- R.getOneAct resId cgacGetActiveGetArticlesCap
  gagaSet <- getSet $ Entity.val gagaa
  let newGagaSet = Set.insert (Entity.id gaCapEnt, Entity.id dgaFCapEnt) gagaSet
  let newGagac = Query $ GetActiveGetArticlesCaps newGagaSet
  R.updateAct resId cgacGetActiveGetArticlesCap newGagac
  pure ()
  where
    getGaActions :: (WithError m) => Action -> m GetArticlesActions
    getGaActions = \case
      Query qAction -> case qAction of
        GetArticles gaActions -> pure gaActions
        _wrongAction -> wrongAction
      _wrongAction -> wrongAction

    getSet :: (WithError m) => Action -> m (Set (Id Capability, Id Capability))
    getSet = \case
      Query qAction -> case qAction of
        GetActiveGetArticlesCaps set -> pure set
        _wrongAction -> wrongAction
      _wrongAction -> wrongAction

    convertPetname :: Maybe Text -> Maybe Text
    convertPetname = \case
      Nothing -> Nothing
      Just petname -> if Text.null petname then Nothing else Just petname

    wrongAction :: (WithError m) => m a
    wrongAction = throwError $ serverError "Wrong action called"

insertAction ::
  (WriteCapabilities m, MonadRandom m) =>
  Id Resource ->
  Action ->
  m (Entity Action)
insertAction resId act = do
  actId <- getRandomId
  R.insertAct resId actId act
  pure $ Entity actId act

insertCapability ::
  (WriteCapabilities m, MonadRandom m) =>
  Id Resource ->
  Capability ->
  m (Entity Capability)
insertCapability resId cap = do
  capId <- getRandomId
  R.insertCap resId capId cap
  pure $ Entity capId cap
