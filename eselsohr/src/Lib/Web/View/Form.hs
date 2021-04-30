module Lib.Web.View.Form
  ( createCollection
  , createGetArticlesCap
  , createArticle
  , changeArticleTitle
  , archiveArticle
  , unreadArticle
  , deleteArticle
  , deleteUnlockLink
  ) where

import           Lib.Core.Domain.Accesstoken    ( Accesstoken )
import           Lib.Core.Domain.ExpirationDate ( ExpirationDate
                                                , expDateToText
                                                )
import qualified Lib.Web.Route.Common          as Route
import           Lucid
import           Servant                        ( Link
                                                , toUrlPiece
                                                )

createCollection :: Html ()
createCollection =
  form_ [linkAbsAction_ Route.createResourceR, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_method", value_ "POST"]
    input_ [type_ "submit", value_ "Create new collection"]

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

postMethodButton :: [Html ()] -> Text -> Accesstoken -> Link -> Html ()
postMethodButton = genPost False "POST" Route.actionR

patchMethodButton :: [Html ()] -> Text -> Accesstoken -> Link -> Html ()
patchMethodButton = genPost False "PATCH" Route.actionR

patchMethodLink :: [Html ()] -> Text -> Accesstoken -> Link -> Html ()
patchMethodLink = genPost True "PATCH" Route.actionR

deleteMethodLink :: Text -> Accesstoken -> Link -> Html ()
deleteMethodLink = genPost True "DELETE" Route.actionR []

createArticle :: Accesstoken -> Link -> Html ()
createArticle = postMethodButton [urlInput] "Save article"
 where
  urlInput = input_ [type_ "text", name_ "articleUri", placeholder_ "URL"]

deleteUnlockLink :: Accesstoken -> Link -> Html ()
deleteUnlockLink = deleteMethodLink "Delete unlock link"

deleteArticle :: Accesstoken -> Link -> Html ()
deleteArticle = deleteMethodLink "Delete article"

createGetArticlesCap
  :: (ExpirationDate, ExpirationDate) -> Accesstoken -> Link -> Html ()
createGetArticlesCap (currTime, expTime) = postMethodButton
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

changeArticleTitle :: Text -> Accesstoken -> Link -> Html ()
changeArticleTitle artTitle = patchMethodButton [changeTitle] "Change title"
 where
  changeTitle =
    input_ [type_ "text", name_ "articleTitle", value_ $ toText artTitle]

archiveArticle :: Accesstoken -> Link -> Html ()
archiveArticle = patchMethodLink [] "Mark as read"

unreadArticle :: Accesstoken -> Link -> Html ()
unreadArticle = patchMethodLink [] "Mark as unread"

linkAbsAction_ :: Link -> Attribute
linkAbsAction_ = action_ . ("/" <>) . toUrlPiece

linkAbsValue_ :: Link -> Attribute
linkAbsValue_ = value_ . ("/" <>) . toUrlPiece
