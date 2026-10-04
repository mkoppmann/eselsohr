{- |
Module      : Eselsohr.Web.Page
Description : The page of a presented bearer

'renderBearerPage' renders the one page of a bearer from the description of its grant and, when the grant has ReadArticles, the articles of its scope.
It is a pure function: the page shows only what the grant permits, and every link and form target is built from the presented bearer text.
Its heading names the grant's scope, a collection or an article.

A grant with ReadArticles sees its articles newest first, each title linking to its URL with its read state and the day it was added; titles and URLs are escaped text.
A grant with AddArticles on the whole collection, as 'mayAddArticles' decides, sees the add form, which posts to @/b/\<bearer\>/add@.
t'PageExtras' adds the fixed notice that an article was submitted, after the redirect of an addition, and the submitted values with a message for each field that failed validation, after an invalid submission; the values appear only as escaped text in the form.
A grant that permits neither sees a page without articles or forms.
An expiring grant also shows its deadline.
-}
module Eselsohr.Web.Page
    ( PageExtras (..)
    , plainPage
    , renderBearerPage
    , mayAddArticles
    ) where

import Control.Monad (when)
import Data.ByteString.Lazy (LazyByteString)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Eselsohr.Article (Article (..), ArticleState (..), renderTitle)
import Eselsohr.Capability (Permission (..), Scope (..))
import Eselsohr.Store (GrantView (..), Revised (..))
import Eselsohr.Web.Form (AddInput (..), FieldProblem (..), emptyAddInput)
import Eselsohr.Web.Html (document)
import Eselsohr.WebUrl (renderWebUrl)
import Lucid

-- | Whether the page shows a submission notice, and the input of a failed submission.
data PageExtras = PageExtras
    { extrasSubmitted :: Bool
    , extrasInput :: AddInput
    }

-- | The page without a notice or submitted values.
plainPage :: PageExtras
plainPage = PageExtras False emptyAddInput

renderBearerPage :: Text -> PageExtras -> GrantView -> Maybe [Revised Article] -> LazyByteString
renderBearerPage presented extras view articles = renderBS . document heading $ do
    when (extrasSubmitted extras) $ p_ "Your article was submitted."
    maybe mempty expiration (viewExpiresAt view)
    when mayAdd (addForm presented (extrasInput extras))
    maybe mempty articleList articles
    when (not mayAdd && null articles) $
        p_ "This link offers no actions on this page."
  where
    mayAdd = mayAddArticles view
    heading = if wholeCollection (viewScope view) then "Collection" else "Article"

-- | Whether the grant may add articles, which needs AddArticles on the whole collection.
mayAddArticles :: GrantView -> Bool
mayAddArticles view = AddArticles `Set.member` viewPermissions view && wholeCollection (viewScope view)

wholeCollection :: Scope -> Bool
wholeCollection = \case
    WholeCollection _ -> True
    OneArticle _ _ -> False

expiration :: UTCTime -> Html ()
expiration deadline = p_ ("This link expires on " <> toHtml (formatTime defaultTimeLocale "%Y-%m-%d at %H:%M UTC" deadline) <> ".")

addForm :: Text -> AddInput -> Html ()
addForm presented input = section_ $ do
    h2_ "Add an article"
    mapM_ (p_ . problemText) (inputProblems input)
    form_ [method_ "post", action_ ("/b/" <> presented <> "/add")] $ do
        p_ $ label_ $ do
            "URL "
            input_ [type_ "url", name_ "url", required_ "", maxlength_ "8192", value_ (inputUrl input)]
        p_ $ label_ $ do
            "Title "
            input_ [type_ "text", name_ "title", required_ "", maxlength_ "4096", value_ (inputTitle input)]
        button_ [type_ "submit"] "Add"

problemText :: FieldProblem -> Html ()
problemText = \case
    InvalidUrl -> "The URL must be a complete http or https address, such as https://example.org/article, without a user name or a port other than the default."
    InvalidTitle -> "The title must contain text, at most 500 characters once spaces are joined, and no control characters."

articleList :: [Revised Article] -> Html ()
articleList articles = section_ $ do
    h2_ "Articles"
    case sortOn (Down . articleCreatedAt) (map revisedValue articles) of
        [] -> p_ "No articles yet."
        sorted -> ul_ (mapM_ articleItem sorted)

articleItem :: Article -> Html ()
articleItem article = li_ $ do
    a_ [href_ (renderWebUrl (articleUrl article)), rel_ "noreferrer noopener"] (toHtml (renderTitle (articleTitle article)))
    " (" <> stateText (articleState article) <> ", added " <> toHtml (day (articleCreatedAt article)) <> ")"
  where
    stateText = \case
        Unread -> "unread"
        Read -> "read"
    day = Text.pack . formatTime defaultTimeLocale "%Y-%m-%d"
