module Routes where

import Data.String (IsString)
import Data.Text.Lazy (Text)
import Web.Scotty (RoutePattern)

capabilitiesRoute :: (Semigroup a, IsString a) => a -> a
capabilitiesRoute cap = "/api/" <> cap

capabilitiesRoutePattern :: RoutePattern
capabilitiesRoutePattern = "/api/:cap"

collectionRoute :: Text
collectionRoute = "/collections"

collectionRoutePattern :: RoutePattern
collectionRoutePattern = "/collections"

collectionWithAccesstokenRoute :: (Semigroup a, IsString a) => a -> a
collectionWithAccesstokenRoute acc = "/collections/" <> acc

collectionWithAccesstokenRoutePattern :: RoutePattern
collectionWithAccesstokenRoutePattern = "/collections/:acc"

collectionCreationRoute :: Text
collectionCreationRoute = "/collections/new"

collectionCreationRoutePattern :: RoutePattern
collectionCreationRoutePattern = "/collections/new"

articlesRoute :: Text
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
