module Lib.Web.View.Form
  ( createCollection,
    createGetArticlesCap,
    createArticle,
    showArticles,
    showArticle,
    editArticle,
    changeArticleTitle,
    archiveArticle,
    unreadArticle,
    deleteArticle,
    deleteUnlockLink,
    refreshButton,
    refreshResourceOverview,
    refreshShowArticles,
    refreshShowArticle,
    backToGetArticlesButton,
  )
where

import Lib.Core.Domain.Accesstoken (Accesstoken)
import Lib.Core.Domain.ExpirationDate (ExpirationDate, expDateToText)
import qualified Lib.Web.Route.Common as Route
import Lucid
import Servant (Link, toUrlPiece)

createCollection :: Html ()
createCollection =
  form_ [linkAbsAction_ Route.createResourceR, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_commandMethod", value_ "POST"]
    input_ [type_ "hidden", name_ "_queryMethod", value_ "GET"]
    input_ [type_ "submit", value_ "Create new collection"]

genPost :: Bool -> Text -> Link -> [Html ()] -> Text -> Accesstoken -> Html ()
genPost asLink commandMethod route inputFields buttonName acc =
  form_ [linkAbsAction_ route, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_commandMethod", value_ commandMethod]
    input_ [type_ "hidden", name_ "_queryMethod", value_ "GET"]
    input_ [type_ "hidden", name_ "acc", value_ $ toUrlPiece acc]
    sequenceA_ inputFields
    if asLink
      then button_ [type_ "submit", class_ "link"] . span_ $ toHtml buttonName
      else input_ [type_ "submit", value_ buttonName]

postMethodButton :: [Html ()] -> Text -> Accesstoken -> Html ()
postMethodButton = genPost False "POST" Route.actionR

patchMethodButton :: [Html ()] -> Text -> Accesstoken -> Html ()
patchMethodButton = genPost False "PATCH" Route.actionR

patchMethodLink :: [Html ()] -> Text -> Accesstoken -> Html ()
patchMethodLink = genPost True "PATCH" Route.actionR

deleteMethodLink :: Text -> Accesstoken -> Html ()
deleteMethodLink = genPost True "DELETE" Route.actionR []

getMethodLink :: Link -> [Html ()] -> Text -> Accesstoken -> Html ()
getMethodLink = genPost True "GET"

createArticle :: Accesstoken -> Html ()
createArticle = postMethodButton [urlInput] "Save article"
  where
    urlInput = input_ [type_ "text", name_ "articleUri", placeholder_ "URL"]

deleteUnlockLink :: Accesstoken -> Html ()
deleteUnlockLink = deleteMethodLink "Delete unlock link"

deleteArticle :: Accesstoken -> Html ()
deleteArticle = deleteMethodLink "Delete article"

showArticles :: Text -> Accesstoken -> Html ()
showArticles = getMethodLink (Route.listArticlesR Nothing) []

showArticle :: Text -> Accesstoken -> Html ()
showArticle = getMethodLink (Route.showArticleR Nothing) []

editArticle :: Text -> Accesstoken -> Html ()
editArticle = getMethodLink (Route.editArticleR Nothing) []

createGetArticlesCap :: (ExpirationDate, ExpirationDate) -> Accesstoken -> Html ()
createGetArticlesCap (currTime, expTime) =
  postMethodButton [options] "Create access link"
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
        [ type_ "datetime-local",
          name_ "expirationDate",
          id_ "expirationDate",
          min_ $ expDateToText currTime,
          value_ $ expDateToText expTime
        ]

changeArticleTitle :: Text -> Accesstoken -> Html ()
changeArticleTitle artTitle = patchMethodButton [changeTitle] "Change title"
  where
    changeTitle =
      input_ [type_ "text", name_ "articleTitle", value_ $ toText artTitle]

archiveArticle :: Accesstoken -> Html ()
archiveArticle = patchMethodLink [] "Mark as read"

unreadArticle :: Accesstoken -> Html ()
unreadArticle = patchMethodLink [] "Mark as unread"

refreshButton :: Link -> Accesstoken -> Html ()
refreshButton route = getMethodLink route [] "Refresh this page"

refreshResourceOverview :: Accesstoken -> Html ()
refreshResourceOverview acc = refreshButton (Route.collectionMainR $ Just acc) acc

refreshShowArticles :: Accesstoken -> Html ()
refreshShowArticles = refreshButton (Route.listArticlesR Nothing)

refreshShowArticle :: Accesstoken -> Html ()
refreshShowArticle = refreshButton (Route.showArticleR Nothing)

backToGetArticlesButton :: Accesstoken -> Html ()
backToGetArticlesButton =
  getMethodLink (Route.listArticlesR Nothing) [] "Back to article list"

linkAbsAction_ :: Link -> Attribute
linkAbsAction_ = action_ . ("/" <>) . toUrlPiece
