module Lib.Core.Flow.Query
  (
  )
where

{- startpage = prepareFirstCap >>= renderPage
  where
    prepareFirstCap = do
      log D "Initializing database"
      HC.initAccDb Res.accColId
      log D "Database created"
      let createCap = UaColAction CreateCollection
      log D "Looking for initial capabilities"
      mCap <- Res.capLookupViaAction Res.accColId createCap
      case mCap of
        Just cap -> do
          log D "Initial capabilities already exist. Using them."
          HC.getAccIdViaCapId Res.accColId $ capabilityId cap
        Nothing -> do
          log D "We need to create our initial capabilities."
          HC.addCapAndAcc Res.accColId Nothing createCap

    renderPage accId = do
      let page = Page.root HC.actionR accId
      pure $ App.render HC.stylesheetR page -}

{- collectionMain ::
  ( RWResource Accesstoken m,
    MonadRandom m,
    WithError m,
    WithLog env m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
collectionMain mAccId = case mAccId of
  Nothing -> throwError $ missingParameter "Acc parameter missing"
  Just accId -> do
    (collId, action) <- HC.getCollIdAndActionViaAccId Res.accColId accId
    case action of
      UaArtAction _notCollectionAction -> throwError $ invalid "Wrong action"
      UaColAction collectionAction -> case collectionAction of
        OverviewCollection -> do
          -- TODO: Get tokens for collection settings and share menu
          listArticlesCapId <- Res.capGetOneViaAction collId $ UaArtAction ListArticles
          listArticlesAccIds <- HC.getAccIdsViaCapId Res.accColId $ capabilityId listArticlesCapId
          deleteAccIds <- HC.getAccIdsForAction collId Nothing $ UaColAction DeleteListArticles
          let activeLinks = zip listArticlesAccIds deleteAccIds
          newListArticleCap <- Res.capGetOneViaAction collId $ UaColAction CreateListArticlesAcc
          newListArticlesAcc <- HC.addAcc collId $ capabilityId newListArticleCap
          let page =
                Page.collectionMain
                  (HC.collectionSettingsR Nothing)
                  (HC.collectionShareR Nothing)
                  HC.actionR
                  (HC.listArticlesR Nothing)
                  activeLinks
                  newListArticlesAcc
          pure $ App.render HC.stylesheetR page
        _wrongCollectionAction -> throwError $ invalid "Wrong action"

collectionSettings :: (WithError m) => Maybe (Id Accesstoken) -> m HtmlPage
collectionSettings mAccId = throwError $ serverError "Action not yet implemented"

collectionShare :: (WithError m) => Maybe (Id Accesstoken) -> m HtmlPage
collectionShare mAccId = throwError $ serverError "Action not yet implemented"

listArticles ::
  ( ReadResource Accesstoken m,
    ReadResource Article m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
listArticles mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    ListArticles -> do
      insertArticleToken <- HC.getAccIdForAction collId Nothing $ UaArtAction InsertArticle
      articlesWithTokens <- getArticlesForRendering collId
      let page =
            Page.articles
              (HC.showArticleR Nothing)
              (HC.editArticleR Nothing)
              HC.actionR
              insertArticleToken
              articlesWithTokens
      pure $ App.render HC.stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

showArticle ::
  ( ReadResource Accesstoken m,
    ReadResource Article m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
showArticle mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    ShowArticle aId -> do
      articleWithTokens <- getArticleForRendering collId aId
      let page = Page.showArticle (HC.editArticleR Nothing) HC.actionR articleWithTokens
      pure $ App.render HC.stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

editArticle ::
  ( ReadResource Accesstoken m,
    ReadResource Article m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
editArticle mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    EditArticle aId -> do
      article <- Res.getOne collId aId
      let changeCap = UaArtAction $ ChangeArticleTitle aId
      editTitleCap <- HC.getAccIdForAction collId (Just $ castId @() aId) changeCap
      let page = Page.editArticle HC.actionR editTitleCap $ articleTitle article
      pure $ App.render HC.stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

genericArticleHandler ::
  ( ReadResource Accesstoken m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m (Id Collection, ArticleAction)
genericArticleHandler mAccId = case mAccId of
  Nothing -> throwError $ missingParameter "Acc parameter missing"
  Just accId -> do
    (collId, action) <- HC.getCollIdAndActionViaAccId Res.accColId accId
    case action of
      UaColAction _notArticleAction -> throwError $ invalid "Wrong action"
      UaArtAction articleAction -> pure (collId, articleAction) -}

{- getArticlesForRendering ::
  ( ReadResource Accesstoken m,
    ReadResource Article m,
    WithError m
  ) =>
  Id Collection ->
  m [ArticleWithTokens]
getArticlesForRendering collId =
  Res.getMany collId
    >>= traverse (getArticleForRendering collId . articleId)

getArticleForRendering ::
  ( ReadResource Accesstoken m,
    ReadResource Article m,
    WithError m
  ) =>
  Id Collection ->
  Id Article ->
  m ArticleWithTokens
getArticleForRendering collId aId = do
  article <- Res.getOne collId aId
  showToken <- tokenFor $ ShowArticle aId
  editToken <- tokenFor $ EditArticle aId
  changeTitleToken <- tokenFor $ ChangeArticleTitle aId
  archiveToken <- tokenFor $ ArchiveArticle aId
  unreadToken <- tokenFor $ UnreadArticle aId
  deleteToken <- tokenFor $ DeleteArticle aId
  return $
    ArticleWithTokens
      article
      showToken
      editToken
      changeTitleToken
      archiveToken
      unreadToken
      deleteToken
  where
    tokenFor =
      HC.getAccIdForAction collId (Just $ castId @() aId) . UaArtAction -}
