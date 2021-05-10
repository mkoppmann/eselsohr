module Lib.Web.View.Form
  ( createCollection
  , createUnlockLink
  , createArticle
  , changeArticleTitle
  , archiveArticle
  , unreadArticle
  , deleteArticle
  , deleteUnlockLink
  ) where

import           Lib.Core.Domain                ( Accesstoken
                                                , Article
                                                , Capability
                                                , ExpirationDate
                                                , Id
                                                , expDateToText
                                                )
import qualified Lib.Web.Route                 as Route
import           Lucid
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
  [options]
  "Create access link"
 where
  options = do
    ul_ $ do
      petname
      expirationDate
  petname = li_ $ do
    label_ [Lucid.for_ "petname"] "Optional name for this token: "
    input_ [type_ "text", name_ "petname", id_ "petname"]
  expirationDate = li_ $ do
    label_ [Lucid.for_ "noExpiration"] "Access link expires on:"
    input_
      [ type_ "datetime-local"
      , name_ "expirationDate"
      , id_ "expirationDate"
      , min_ $ expDateToText currTime
      , value_ $ expDateToText expTime
      ]

deleteUnlockLink :: Id Capability -> Accesstoken -> Link -> Html ()
deleteUnlockLink unlockLinkId = deleteMethodLink
  (fieldLink Route.deleteUnlockLink unlockLinkId)
  []
  "Delete unlock link"

createArticle :: Accesstoken -> Link -> Html ()
createArticle = postMethodButton (fieldLink Route.createArticle)
                                 [urlInput]
                                 "Save article"
 where
  urlInput = input_ [type_ "text", name_ "articleUri", placeholder_ "URL"]

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
