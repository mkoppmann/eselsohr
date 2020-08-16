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

articlesRoute :: (Semigroup a, IsString a) => a -> a
articlesRoute acc = "/collections/" <> acc <> "/articles"

articlesRoutePattern :: RoutePattern
articlesRoutePattern = "/collections/:acc/articles"

articleWithIdRoute :: (Semigroup a, IsString a) => a -> a -> a
articleWithIdRoute acc aId = "/collections/" <> acc <> "/articles/" <> aId

articleWithIdRoutePattern :: RoutePattern
articleWithIdRoutePattern = "/collections/:acc/articles/:id"

editArticleWithIdRoute :: (Semigroup a, IsString a) => a -> a -> a
editArticleWithIdRoute acc aId = "/collections/" <> acc <> "/articles/" <> aId <> "/edit"

editArticleWithIdRoutePattern :: RoutePattern
editArticleWithIdRoutePattern = "/collections/:acc/articles/:id/edit"
