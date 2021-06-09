module Lib.Web.View.Form
  ( createCollection
  , createUnlockLink
  , createSharedOverviewRef
  , createArticle
  , changeArticleTitle
  , archiveArticle
  , unreadArticle
  , deleteArticle
  , deleteUnlockLink
  , createSharedArticlesRef
  , createSharedArticleRef
  , deleteSharedReference
  ) where

import           Lib.Core.Domain                ( Accesstoken
                                                , Article
                                                , Capability
                                                , ExpirationDate
                                                , Id
                                                , expDateToText
                                                )
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( CreateSharedArticleRefPerms(..)
                                                , CreateSharedArticlesRefPerms(..)
                                                , CreateSharedOverviewRefPerms(..)
                                                )
import           Lucid
import           Prelude                 hiding ( for_ )
import           Servant                        ( Link
                                                , fieldLink
                                                , toUrlPiece
                                                )

createCollection :: Html ()
createCollection =
  form_ [linkAbsAction_ $ fieldLink Route.createCollection, method_ "POST"] $ do
    input_ [type_ "submit", value_ "Create new collection"]

createUnlockLink
  :: ExpirationDate -> ExpirationDate -> Accesstoken -> Link -> Html ()
createUnlockLink currTime expTime = postMethodButton
  (fieldLink Route.createUnlockLink)
  [createLink currTime expTime]
  "Create access link"

deleteUnlockLink :: Id Capability -> Accesstoken -> Link -> Html ()
deleteUnlockLink unlockLinkId = deleteMethodLink
  (fieldLink Route.deleteUnlockLink unlockLinkId)
  []
  "Delete unlock link"

createSharedOverviewRef
  :: CreateSharedOverviewRefPerms
  -> ExpirationDate
  -> ExpirationDate
  -> Accesstoken
  -> Link
  -> Html ()
createSharedOverviewRef CreateSharedOverviewRefPerms {..} currTime expTime =
  postMethodButton (fieldLink Route.createSharedOverviewRef)
                   [createLinkWithSharing currTime expTime permissionList]
                   "Create link"
 where
  permissionList =
    [ when canViewUnlockLinks
      $ permissionField "viewUnlockLinks" "View unlock links"
    , when canCreateUnlockLinks
      $ permissionField "createUnlockLinks" "Create unlock links"
    , when canDeleteUnlockLinks $ permissionField "delete" "Delete unlock links"
    ]

createArticle :: Accesstoken -> Link -> Html ()
createArticle = postMethodButton (fieldLink Route.createArticle)
                                 [urlInput]
                                 "Save article"
  where urlInput = input_ [type_ "url", name_ "articleUri", placeholder_ "URL"]

changeArticleTitle :: Id Article -> Text -> Accesstoken -> Link -> Html ()
changeArticleTitle artId artTitle = patchMethodButton
  (fieldLink Route.patchArticle artId)
  [changeTitle]
  "Change title"
 where
  changeTitle =
    input_ [type_ "text", name_ "articleTitle", value_ $ toText artTitle]

archiveArticle :: Id Article -> Accesstoken -> Link -> Html ()
archiveArticle artId = patchMethodLink (fieldLink Route.patchArticle artId)
                                       [archive]
                                       "Mark as read"
 where
  archive = input_ [type_ "hidden", name_ "articleState", value_ "Archived"]

unreadArticle :: Id Article -> Accesstoken -> Link -> Html ()
unreadArticle artId = patchMethodLink (fieldLink Route.patchArticle artId)
                                      [unread]
                                      "Mark as unread"
  where unread = input_ [type_ "hidden", name_ "articleState", value_ "Unread"]

deleteArticle :: Id Article -> Accesstoken -> Link -> Html ()
deleteArticle artId =
  deleteMethodLink (fieldLink Route.deleteArticle artId) [] "Delete article"

createSharedArticlesRef
  :: CreateSharedArticlesRefPerms
  -> ExpirationDate
  -> ExpirationDate
  -> Accesstoken
  -> Link
  -> Html ()
createSharedArticlesRef CreateSharedArticlesRefPerms {..} currTime expTime =
  postMethodButton (fieldLink Route.createSharedArticlesRef)
                   [createLinkWithSharing currTime expTime permissionList]
                   "Create link"
 where
  permissionList =
    [ when canViewArticles $ permissionField "viewArticles" "View articles"
    , when canCreateArticles
      $ permissionField "createArticles" "Create articles"
    , when canChangeArticleTitle $ permissionField "changeTitle" "Change titles"
    , when canChangeArticleState $ permissionField "changeState" "Change states"
    , when canDeleteArticle $ permissionField "delete" "Delete articles"
    ]

createSharedArticleRef
  :: Id Article
  -> CreateSharedArticleRefPerms
  -> ExpirationDate
  -> ExpirationDate
  -> Accesstoken
  -> Link
  -> Html ()
createSharedArticleRef artId CreateSharedArticleRefPerms {..} currTime expTime
  = postMethodButton (fieldLink Route.createSharedArticleRef artId)
                     [createLinkWithSharing currTime expTime permissionList]
                     "Create link"
 where
  permissionList =
    [ when canViewArticle $ permissionField "viewArticle" "View article"
    , when canChangeArticleTitle $ permissionField "changeTitle" "Change title"
    , when canChangeArticleState $ permissionField "changeState" "Change state"
    , when canDeleteArticle $ permissionField "delete" "Delete articles"
    ]

deleteSharedReference :: Id Capability -> Accesstoken -> Link -> Html ()
deleteSharedReference sharedLink = deleteMethodLink
  (fieldLink Route.deleteSharedRef sharedLink)
  []
  "Delete shared link"

-- Helpers

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

createLinkWithSharing
  :: ExpirationDate -> ExpirationDate -> [Html ()] -> Html ()
createLinkWithSharing currTime expTime permissionList =
  ul_ [class_ "no-bullet"] $ do
    li_ petnameField
    li_ $ expirationDateField currTime expTime
    li_ $ sharingOptions permissionList

petnameField :: Html ()
petnameField = div_ [class_ "form-group"] $ do
  label_ [for_ "petname"] "Optional name for this token"
  input_ [type_ "text", id_ "petname", name_ "petname", id_ "petname"]

expirationDateField :: ExpirationDate -> ExpirationDate -> Html ()
expirationDateField currTime expTime = div_ [class_ "form-group"] $ do
  label_ [for_ "expirationDate"] "Access link expires on"
  input_
    [ type_ "datetime-local"
    , name_ "expirationDate"
    , id_ "expirationDate"
    , min_ $ expDateToText currTime
    , value_ $ expDateToText expTime
    ]

createLink :: ExpirationDate -> ExpirationDate -> Html ()
createLink currTime expTime = ul_ [class_ "no-bullet"] $ do
  li_ petnameField
  li_ $ expirationDateField currTime expTime

sharingOptions :: [Html ()] -> Html ()
sharingOptions permissionList = fieldset_ $ do
  legend_ "Permissions"
  ul_ $ traverse_ li_ permissionList

permissionField :: Text -> Text -> Html ()
permissionField fieldName labelText = do
  input_ [type_ "checkbox", id_ fieldName, name_ fieldName, value_ "Allowed"]
  label_ [for_ fieldName] $ toHtml labelText
