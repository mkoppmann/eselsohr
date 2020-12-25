module Lib.Web.View.Form
  ( newCollection,
    newListArticles,
    newArticle,
    showArticle,
    editArticle,
    changeArticleTitle,
    archiveArticle,
    unreadArticle,
    deleteArticle,
  )
where

import qualified Data.UUID as UUID
import Lib.Core.Accesstoken (Accesstoken)
import Lib.Core.Id (Id (unId))
import Lib.Core.Uuid (Uuid (unUuid))
import Lucid
import Servant (Link, toUrlPiece)

newCollection :: Link -> Id Accesstoken -> Html ()
newCollection actionRoute acc =
  form_ [linkAbsAction_ actionRoute, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "postafAccesstoken", value_ $ accText acc]
    input_ [type_ "submit", value_ "Create new collection"]

newListArticles :: Link -> Id Accesstoken -> Html ()
newListArticles actionRoute acc =
  form_ [linkAbsAction_ actionRoute, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "postafAccesstoken", value_ $ accText acc]
    ul_ $ do
      li_ $ do
        label_ [Lucid.for_ "secureLink"] "Secure links:"
        input_ [type_ "checkbox", name_ "postafSecureLink", id_ "secureLink", value_ "True"]
      li_ $ do
        label_ [Lucid.for_ "noExpiration"] "Access link does not expire:"
        input_ [type_ "checkbox", name_ "postafNoExpiration", id_ "noExpiration", value_ "True"]
    input_ [type_ "submit", value_ "Create access link"]

newArticle :: Link -> Id Accesstoken -> Html ()
newArticle actionRoute acc =
  form_ [linkAbsAction_ actionRoute, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "postafAccesstoken", value_ $ accText acc]
    input_ [type_ "text", name_ "postafUri", placeholder_ "URL"]
    input_ [type_ "submit", value_ "Save article"]

showArticle :: Link -> Id Accesstoken -> LText -> Html ()
showArticle actionRoute acc aTitle =
  form_ [linkAbsAction_ actionRoute, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_method", value_ "GET"]
    input_ [type_ "hidden", name_ "acc", value_ $ accText acc]
    button_ [type_ "submit", class_ "link"] . span_ $ toHtml aTitle

editArticle :: Link -> Id Accesstoken -> Html ()
editArticle actionRoute acc =
  form_ [linkAbsAction_ actionRoute, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_method", value_ "GET"]
    input_ [type_ "hidden", name_ "acc", value_ $ accText acc]
    button_ [type_ "submit", class_ "link"] $ span_ "Edit title"

changeArticleTitle :: Link -> Id Accesstoken -> LText -> Html ()
changeArticleTitle actionRoute acc aTitle =
  form_ [linkAbsAction_ actionRoute, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_method", value_ "PATCH"]
    input_ [type_ "hidden", name_ "patchafAccesstoken", value_ $ accText acc]
    input_ [type_ "text", name_ "patchafArticleTitle", value_ $ toText aTitle]
    button_ [type_ "submit", class_ "link"] $ span_ "Save new title"

archiveArticle :: Link -> Id Accesstoken -> Html ()
archiveArticle = changeArticleState "Mark as read"

unreadArticle :: Link -> Id Accesstoken -> Html ()
unreadArticle = changeArticleState "Mark as unread"

deleteArticle :: Link -> Id Accesstoken -> Html ()
deleteArticle actionRoute acc =
  form_ [linkAbsAction_ actionRoute, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_method", value_ "DELETE"]
    input_ [type_ "hidden", name_ "deleteafAccesstoken", value_ $ accText acc]
    button_ [type_ "submit", class_ "link"] $ span_ "Delete article"

changeArticleState :: Text -> Link -> Id Accesstoken -> Html ()
changeArticleState buttonText actionRoute acc =
  form_ [linkAbsAction_ actionRoute, method_ "POST"] $ do
    input_ [type_ "hidden", name_ "_method", value_ "PATCH"]
    input_ [type_ "hidden", name_ "patchafAccesstoken", value_ $ accText acc]
    button_ [type_ "submit", class_ "link"] . span_ $ toHtml buttonText

linkAbsAction_ :: Link -> Attribute
linkAbsAction_ = action_ . ("/" <>) . toUrlPiece

accText :: Id Accesstoken -> Text
accText = UUID.toText . unUuid . unId
