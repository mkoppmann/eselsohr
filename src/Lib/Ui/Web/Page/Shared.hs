module Lib.Ui.Web.Page.Shared
  (
    -- * Query related
    WithQuery
  , lookupReferences
  , getArticleListPm
  , getArticleMap
  , getArticle
  , getCapabilityListPm
  , getCapabilityMap
  , capStillValid
  , getExpirationDates
  , getSharedLinks

    -- * HTML related
  , navBar
  , expirationDate
  , prettyDate

    -- ** HTML form related
  , createCollectionForm
  , createUnlockLinkForm
  , deleteUnlockLinkForm
  , createSharedOverviewRefForm
  , deleteSharedOverviewRefForm
  , createArticleForm
  , changeArticleTitleForm
  , markArticleAsReadForm
  , markArticleAsUnreadForm
  , deleteArticleForm
  , createSharedArticleListRefForm
  , deleteSharedArticleListRefForm
  , createSharedArticleRefForm
  , deleteSharedArticleRefForm
  ) where

import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq

import           Data.Time                      ( UTCTime(..)
                                                , addGregorianMonthsClip
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           Lucid
import           Prelude                 hiding ( for_ )
import           Servant                        ( Link
                                                , fieldLink
                                                , toUrlPiece
                                                )

import qualified Lib.App.Port                  as Port
import qualified Lib.Domain.Capability         as Capability
import qualified Lib.Infra.Persistence.File    as File
import qualified Lib.Infra.Persistence.Model.Article
                                               as ArtPm
import qualified Lib.Infra.Persistence.Model.Capability
                                               as CapPm
import qualified Lib.Infra.Persistence.Model.Collection
                                               as ColPm
import qualified Lib.Ui.Web.Page.ViewModel.Article
                                               as ArticleVm
import qualified Lib.Ui.Web.Page.ViewModel.UnlockLink
                                               as UnlockLink
import qualified Lib.Ui.Web.Route              as Route

import           Lib.App.Port                   ( MonadTime )
import           Lib.Domain.Article             ( Article )
import           Lib.Domain.Capability          ( Capability
                                                , ObjectReference
                                                )
import           Lib.Domain.Collection          ( Collection )
import           Lib.Domain.Error               ( notAuthorized
                                                , notFound
                                                )
import           Lib.Domain.Id                  ( Id )
import           Lib.Infra.Error                ( WithError
                                                , throwOnError
                                                , throwOnNothing
                                                )
import           Lib.Infra.Log                  ( WithLog )
import           Lib.Infra.Persistence.File     ( WithFile )
import           Lib.Infra.Persistence.Model.ArticleList
                                                ( ArticleListPm )
import           Lib.Infra.Persistence.Model.Capability
                                                ( CapabilityPm )
import           Lib.Infra.Persistence.Model.CapabilityList
                                                ( CapabilityListPm )
import           Lib.Ui.Web.Dto.Accesstoken     ( Accesstoken
                                                , Reference(..)
                                                , toReference
                                                )
import           Lib.Ui.Web.Dto.Id              ( )
import           Lib.Ui.Web.Page.ViewModel.Article
                                                ( ArticleVm )
import           Lib.Ui.Web.Page.ViewModel.Permission
                                                ( ArticlePermsVm(..)
                                                , ArticlesPermsVm(..)
                                                , OverviewPermsVm(..)
                                                )
import           Lib.Ui.Web.Page.ViewModel.UnlockLink
                                                ( UnlockLinkVm )

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

type WithQuery env m
  = (WithFile env m, MonadTime m, WithError m, WithLog env m)

type QueryMap a = Map (Id a) a

lookupReferences
  :: WithQuery env m => Accesstoken -> m (Reference, ObjectReference)
lookupReferences acc = do
  let ref@(Reference colId capId) = toReference acc
  authList   <- getCapabilityListPm colId
  capPm      <- capOrNotAuthError $ Map.lookup capId authList
  curTime    <- Port.getCurrentTime
  validCapPm <- capOrNotAuthError $ capStillValid curTime capPm
  cap        <- throwOnError $ CapPm.toDomain validCapPm
  pure (ref, Capability.objectReference cap)

getArticleListPm :: (WithFile env m) => Id Collection -> m ArticleListPm
getArticleListPm colId = File.load colId ColPm.articleList

getArticleMap :: (WithQuery env m) => Id Collection -> m (QueryMap Article)
getArticleMap = throwOnError . traverse ArtPm.toDomain <=< getArticleListPm

getArticle :: WithQuery env m => Id Collection -> Id Article -> m ArticleVm
getArticle colId artId = do
  artPmMap <- getArticleListPm colId
  artPm    <- notFoundError $ Map.lookup artId artPmMap
  art      <- throwOnError $ ArtPm.toDomain artPm
  pure $ ArticleVm.fromDomain art

getCapabilityListPm :: (WithFile env m) => Id Collection -> m CapabilityListPm
getCapabilityListPm colId = File.load colId ColPm.capabilityList

getCapabilityMap
  :: (WithQuery env m) => Id Collection -> m (QueryMap Capability)
getCapabilityMap =
  throwOnError . traverse CapPm.toDomain <=< getCapabilityListPm

capOrNotAuthError :: (WithError m) => Maybe a -> m a
capOrNotAuthError = throwOnNothing $ notAuthorized "No valid capability found"

capStillValid :: UTCTime -> CapabilityPm -> Maybe CapabilityPm
capStillValid curTime cap = case CapPm.expirationDate cap of
  Nothing      -> Just cap
  Just expDate -> if expDate < curTime then Nothing else Just cap

notFoundError :: (WithError m) => Maybe a -> m a
notFoundError = throwOnNothing notFound

getExpirationDates :: (MonadTime m) => m (UTCTime, UTCTime)
getExpirationDates = do
  currTime@UTCTime {..} <- Port.getCurrentTime
  let expDate = currTime { utctDay = addGregorianMonthsClip 1 utctDay }
  pure (currTime, expDate)

getSharedLinks
  :: (WithQuery env m)
  => Id Collection
  -> (ObjectReference -> Bool)
  -> m (Seq UnlockLinkVm)
getSharedLinks colId sharedRefFilter = do
  capPmMap <- getCapabilityListPm colId
  caps     <- throwOnError (traverse CapPm.toDomain capPmMap)
  pure $ unlockLinkVms <$> filteredCaps caps
 where
  filteredCaps :: Map a Capability -> Seq (a, Capability)
  filteredCaps = Seq.filter (filterF . snd) . Seq.fromList . Map.toList

  filterF :: Capability -> Bool
  filterF = sharedRefFilter . Capability.objectReference

  unlockLinkVms :: (Id Capability, Capability) -> UnlockLinkVm
  unlockLinkVms = uncurry (UnlockLink.fromDomain colId)

------------------------------------------------------------------------
-- HTML
------------------------------------------------------------------------

navBar :: [Html ()] -> Html ()
navBar links = nav_ . ul_ [class_ "no-bullet"] $ traverse_ li_ links

expirationDate :: UTCTime -> Text
expirationDate = toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M"

prettyDate :: UTCTime -> Text
prettyDate = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

------------------------------------------------------------------------
-- HTML forms
------------------------------------------------------------------------

-- Collection related

createCollectionForm :: Html ()
createCollectionForm =
  form_ [linkAbsAction_ $ fieldLink Route.createCollection, method_ "POST"] $ do
    input_ [type_ "submit", value_ "Create new collection"]

createUnlockLinkForm :: UTCTime -> UTCTime -> Accesstoken -> Link -> Html ()
createUnlockLinkForm currTime expTime = postMethodButton
  (fieldLink Route.createUnlockLink)
  [createLink currTime expTime]
  "Create access link"

deleteUnlockLinkForm :: Id Capability -> Accesstoken -> Link -> Html ()
deleteUnlockLinkForm unlockLinkId = deleteMethodLink
  (fieldLink Route.deleteUnlockLink unlockLinkId)
  []
  "Delete unlock link"

createSharedOverviewRefForm
  :: OverviewPermsVm -> UTCTime -> UTCTime -> Accesstoken -> Link -> Html ()
createSharedOverviewRefForm OverviewPermsVm {..} currTime expTime =
  postMethodButton (fieldLink Route.createSharedOverviewRef)
                   [createLinkWithSharing currTime expTime permissionList]
                   "Create link"
 where
  permissionList =
    [ when viewUnlockLinksPermVm
      $ permissionField "viewUnlockLinks" "View unlock links"
    , when createUnlockLinksPermVm
      $ permissionField "createUnlockLinks" "Create unlock links"
    , when deleteUnlockLinksPermVm
      $ permissionField "delete" "Delete unlock links"
    ]

deleteSharedOverviewRefForm :: Id Capability -> Accesstoken -> Link -> Html ()
deleteSharedOverviewRefForm sharedOverviewIdRef = deleteMethodLink
  (fieldLink Route.deleteSharedOverviewRef sharedOverviewIdRef)
  []
  "Delete shared overview link"

-- Article related

createArticleForm :: Accesstoken -> Link -> Html ()
createArticleForm = postMethodButton (fieldLink Route.createArticle)
                                     [urlInput]
                                     "Save article"
  where urlInput = input_ [type_ "url", name_ "articleUri", placeholder_ "URL"]

changeArticleTitleForm :: Id Article -> Text -> Accesstoken -> Link -> Html ()
changeArticleTitleForm artId artTitle = patchMethodButton
  (fieldLink Route.changeArticleTitle artId)
  [changeTitle]
  "Change title"
 where
  changeTitle =
    input_ [type_ "text", name_ "articleTitle", value_ $ toText artTitle]

markArticleAsReadForm :: Id Article -> Accesstoken -> Link -> Html ()
markArticleAsReadForm artId = patchMethodLink
  (fieldLink Route.markArticleAsRead artId)
  [archive]
  "Mark as read"
 where
  archive = input_ [type_ "hidden", name_ "articleState", value_ "Archived"]

markArticleAsUnreadForm :: Id Article -> Accesstoken -> Link -> Html ()
markArticleAsUnreadForm artId = patchMethodLink
  (fieldLink Route.markArticleAsUnread artId)
  [unread]
  "Mark as unread"
  where unread = input_ [type_ "hidden", name_ "articleState", value_ "Unread"]

deleteArticleForm :: Id Article -> Accesstoken -> Link -> Html ()
deleteArticleForm artId =
  deleteMethodLink (fieldLink Route.deleteArticle artId) [] "Delete article"

createSharedArticleListRefForm
  :: ArticlesPermsVm -> UTCTime -> UTCTime -> Accesstoken -> Link -> Html ()
createSharedArticleListRefForm ArticlesPermsVm {..} currTime expTime =
  postMethodButton (fieldLink Route.createSharedArticleListRef)
                   [createLinkWithSharing currTime expTime permissionList]
                   "Create link"
 where
  permissionList =
    [ when viewArticlesPermVm $ permissionField "viewArticles" "View articles"
    , when createArticlesPermVm
      $ permissionField "createArticles" "Create articles"
    , when changeTitlesPermVm $ permissionField "changeTitle" "Change titles"
    , when changeStatesPermVm $ permissionField "changeState" "Change states"
    , when deleteArticlesPermVm $ permissionField "delete" "Delete articles"
    ]

deleteSharedArticleListRefForm
  :: Id Capability -> Accesstoken -> Link -> Html ()
deleteSharedArticleListRefForm sharedArticleListIdRef = deleteMethodLink
  (fieldLink Route.deleteSharedArticleListRef sharedArticleListIdRef)
  []
  "Delete shared article list link"

createSharedArticleRefForm
  :: Id Article
  -> ArticlePermsVm
  -> UTCTime
  -> UTCTime
  -> Accesstoken
  -> Link
  -> Html ()
createSharedArticleRefForm artId ArticlePermsVm {..} currTime expTime =
  postMethodButton (fieldLink Route.createSharedArticleRef artId)
                   [createLinkWithSharing currTime expTime permissionList]
                   "Create link"
 where
  permissionList =
    [ when viewArticlePermVm $ permissionField "viewArticle" "View article"
    , when changeTitlePermVm $ permissionField "changeTitle" "Change title"
    , when changeStatePermVm $ permissionField "changeState" "Change state"
    , when deleteArticlePermVm $ permissionField "delete" "Delete articles"
    ]

deleteSharedArticleRefForm
  :: Id Article -> Id Capability -> Accesstoken -> Link -> Html ()
deleteSharedArticleRefForm articleId sharedArticleIdRef = deleteMethodLink
  (fieldLink Route.deleteSharedArticleRef articleId sharedArticleIdRef)
  []
  "Delete shared article link"

-- Helper

genPost
  :: Bool -> Text -> Link -> [Html ()] -> Text -> Accesstoken -> Link -> Html ()
genPost asLink commandMethod route inputFields buttonName acc gotoUrl =
  form_ [linkAbsAction_ route, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_method", value_ commandMethod]
    input_ [type_ "hidden", name_ "acc", value_ $ toUrlPiece acc]
    input_ [type_ "hidden", name_ "goto", linkAbsValue_ gotoUrl]
    sequenceA_ inputFields
    if asLink
      then button_ [type_ "submit", class_ "link"] . span_ $ toHtml buttonName
      else input_ [type_ "submit", value_ buttonName]

postMethodButton :: Link -> [Html ()] -> Text -> Accesstoken -> Link -> Html ()
postMethodButton = genPost False "POST"

patchMethodButton
  :: Link -> [Html ()] -> Text -> Accesstoken -> Link -> Html ()
patchMethodButton = genPost False "PATCH"

patchMethodLink :: Link -> [Html ()] -> Text -> Accesstoken -> Link -> Html ()
patchMethodLink = genPost True "PATCH"

deleteMethodLink :: Link -> [Html ()] -> Text -> Accesstoken -> Link -> Html ()
deleteMethodLink = genPost True "DELETE"

linkAbsAction_ :: Link -> Attribute
linkAbsAction_ = action_ . ("/" <>) . toUrlPiece

linkAbsValue_ :: Link -> Attribute
linkAbsValue_ = value_ . ("/" <>) . toUrlPiece

createLinkWithSharing :: UTCTime -> UTCTime -> [Html ()] -> Html ()
createLinkWithSharing currTime expTime permissionList =
  ul_ [class_ "no-bullet"] $ do
    li_ petnameField
    li_ $ expirationDateField currTime expTime
    li_ $ sharingOptions permissionList

petnameField :: Html ()
petnameField = div_ [class_ "form-group"] $ do
  label_ [for_ "petname"] "Optional name for this token"
  input_ [type_ "text", id_ "petname", name_ "petname"]

expirationDateField :: UTCTime -> UTCTime -> Html ()
expirationDateField currTime expTime = div_ [class_ "form-group"] $ do
  label_ [for_ "expirationDate"] "Access link expires on"
  input_
    [ type_ "datetime-local"
    , name_ "expirationDate"
    , id_ "expirationDate"
    , min_ $ expirationDate currTime
    , value_ $ expirationDate expTime
    ]

createLink :: UTCTime -> UTCTime -> Html ()
createLink currTime expTime = ul_ [class_ "no-bullet"] $ do
  li_ petnameField
  li_ $ expirationDateField currTime expTime

sharingOptions :: [Html ()] -> Html ()
sharingOptions permissionList = fieldset_ $ do
  legend_ "Permissions"
  ul_ $ traverse_ li_ permissionList

permissionField :: Text -> Text -> Html ()
permissionField fieldName labelText = do
  label_ [for_ fieldName] $ toHtml labelText
  input_ [type_ "checkbox", id_ fieldName, name_ fieldName, value_ "True"]
