module Lib.Web.Route.Common
  ( -- * Route helpers
    collectionMainR,
    listArticlesR,
    showArticleR,
    editArticleR,
    invalidTokenR,
    stylesheetR,
    actionR,
    linkAsText,
  )
where

import Lib.Core.Domain (Accesstoken)
import Lib.Web.Route.Frontend (FrontendSite (..))
import Servant (Link, fieldLink, linkURI)

-- Route helpers

collectionMainR :: Maybe Accesstoken -> Link
collectionMainR = fieldLink collectionMain

listArticlesR :: Maybe Accesstoken -> Link
listArticlesR = fieldLink listArticles

showArticleR :: Maybe Accesstoken -> Link
showArticleR = fieldLink showArticle

editArticleR :: Maybe Accesstoken -> Link
editArticleR = fieldLink editArticle

invalidTokenR :: Link
invalidTokenR = fieldLink invalidToken

stylesheetR :: Link
stylesheetR = fieldLink stylesheet

actionR :: Link
actionR = fieldLink postFrontend

linkAsText :: Link -> Text
linkAsText = (<>) "/" . show @Text . linkURI
