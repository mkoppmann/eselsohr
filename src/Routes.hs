module Routes where

import Web.Scotty (RoutePattern)

cssRoute :: LText
cssRoute = "/static/style.css"

cssRoutePattern :: RoutePattern
cssRoutePattern = "/static/style.css"

capabilitiesRoute :: (Semigroup a, IsString a) => a -> a
capabilitiesRoute cap = "/api/" <> cap

capabilitiesRoutePattern :: RoutePattern
capabilitiesRoutePattern = "/api/:cap"

collectionRoute :: LText
collectionRoute = "/collections"

collectionRoutePattern :: RoutePattern
collectionRoutePattern = "/collections"

collectionWithAccesstokenRoute :: (Semigroup a, IsString a) => a -> a
collectionWithAccesstokenRoute acc = "/collections/" <> acc

collectionWithAccesstokenRoutePattern :: RoutePattern
collectionWithAccesstokenRoutePattern = "/collections/:acc"

collectionCreationRoute :: LText
collectionCreationRoute = "/collections/new"

collectionCreationRoutePattern :: RoutePattern
collectionCreationRoutePattern = "/collections/new"

articlesRoute :: LText
articlesRoute = "/articles"

articlesRoutePattern :: RoutePattern
articlesRoutePattern = "/articles"

articleWithIdRoute :: (Semigroup a, IsString a) => a -> a
articleWithIdRoute aId = "/article/" <> aId

articleWithIdRoutePattern :: RoutePattern
articleWithIdRoutePattern = "/article/:id"

editArticleWithIdRoute :: (Semigroup a, IsString a) => a -> a
editArticleWithIdRoute aId = "/article/" <> aId <> "/edit"

editArticleWithIdRoutePattern :: RoutePattern
editArticleWithIdRoutePattern = "/article/:id/edit"
