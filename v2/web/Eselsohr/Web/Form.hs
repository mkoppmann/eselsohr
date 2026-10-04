{- |
Module      : Eselsohr.Web.Form
Description : Decoding and validating the add form

'decodeAddForm' reads the @url@ and @title@ fields of an @application/x-www-form-urlencoded@ body.
A missing field counts as empty, other fields such as @_method@ or @goto@ are ignored, and a field that is not UTF-8 text makes the whole body undecodable.

'validateAdd' checks both values with the domain constructors, after trimming surrounding whitespace from the URL, and reports every field that fails as a t'FieldProblem'.
An t'AddInput' keeps the submitted text and its problems so that the add form can show them again; 'emptyAddInput' is the form before any submission.
-}
module Eselsohr.Web.Form
    ( AddInput (..)
    , FieldProblem (..)
    , emptyAddInput
    , decodeAddForm
    , validateAdd
    ) where

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import Eselsohr.Article (Title, mkTitle)
import Eselsohr.WebUrl (WebUrl, parseWebUrl)
import Network.HTTP.Types (parseSimpleQuery)

data FieldProblem = InvalidUrl | InvalidTitle
    deriving stock (Eq, Show)

data AddInput = AddInput
    { inputUrl :: Text
    , inputTitle :: Text
    , inputProblems :: [FieldProblem]
    }
    deriving stock (Eq, Show)

emptyAddInput :: AddInput
emptyAddInput = AddInput "" "" []

-- | The URL and title fields, or 'Nothing' when a field is not UTF-8 text.
decodeAddForm :: ByteString -> Maybe (Text, Text)
decodeAddForm body = (,) <$> field "url" <*> field "title"
  where
    fields = parseSimpleQuery body
    field name = maybe (Just "") (either (const Nothing) Just . decodeUtf8') (lookup name fields)

validateAdd :: Text -> Text -> Either [FieldProblem] (WebUrl, Title)
validateAdd url title = case (parseWebUrl (Text.strip url), mkTitle title) of
    (Right validUrl, Right validTitle) -> Right (validUrl, validTitle)
    (urlResult, titleResult) -> Left ([InvalidUrl | isLeft urlResult] <> [InvalidTitle | isLeft titleResult])
