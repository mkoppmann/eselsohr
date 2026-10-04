{- |
Module      : Eselsohr.Web.Route
Description : The route table of the web interface

'route' maps a request method and the decoded path segments to a t'Route', or to 'Nothing' for every request outside the table:

> GET  /b/<bearer>           BearerPage
> POST /b/<bearer>/add       BearerAction AddArticle
> POST /b/<bearer>/title     BearerAction ChangeTitle
> POST /b/<bearer>/state     BearerAction ChangeReadState
> POST /b/<bearer>/delete    BearerAction DeleteArticle
> GET  /deleted              DeletionNotice

HEAD matches wherever GET does.
Each path has exactly one form, so a trailing slash, which WAI decodes as a final empty segment, or an extra segment matches nothing.
The bearer segment stays raw text; callers parse it before opening any collection, and a request that matches nothing receives the invalid-link page.
-}
module Eselsohr.Web.Route
    ( Route (..)
    , Action (..)
    , route
    ) where

import Data.Text (Text)
import Network.HTTP.Types (Method)

data Route
    = BearerPage Text
    | BearerAction Text Action
    | DeletionNotice
    deriving stock (Eq, Show)

-- | A mutation submitted to a bearer through an explicit POST route.
data Action = AddArticle | ChangeTitle | ChangeReadState | DeleteArticle
    deriving stock (Eq, Show)

route :: Method -> [Text] -> Maybe Route
route method segments = case segments of
    ["b", bearer] | readOnly -> Just (BearerPage bearer)
    ["b", bearer, verb] | method == "POST" -> BearerAction bearer <$> action verb
    ["deleted"] | readOnly -> Just DeletionNotice
    _ -> Nothing
  where
    readOnly = method == "GET" || method == "HEAD"

action :: Text -> Maybe Action
action = \case
    "add" -> Just AddArticle
    "title" -> Just ChangeTitle
    "state" -> Just ChangeReadState
    "delete" -> Just DeleteArticle
    _ -> Nothing
