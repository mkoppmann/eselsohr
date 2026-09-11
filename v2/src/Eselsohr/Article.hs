{-# LANGUAGE OverloadedStrings #-}

module Eselsohr.Article
    ( Title
    , TitleError (..)
    , mkTitle
    , renderTitle
    , ArticleState (..)
    , Article (..)
    , newArticle
    , rename
    , setReadState
    ) where

import Data.Char (isControl, isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Eselsohr.Id (ArticleId)
import Eselsohr.WebUrl (WebUrl)

newtype Title = Title Text
    deriving stock (Eq, Show)

data TitleError
    = TitleInputTooLong
    | EmptyTitle
    | TitleTooLong
    | InvalidTitleCharacter
    deriving stock (Eq, Show)

-- | Normalize Unicode whitespace without changing other characters.
mkTitle :: Text -> Either TitleError Title
mkTitle input
    | Text.compareLength input 4096 == GT = Left TitleInputTooLong
    | Text.any (\c -> isControl c && not (isWhitespace c)) input = Left InvalidTitleCharacter
    | Text.null normalized = Left EmptyTitle
    | Text.compareLength normalized 500 == GT = Left TitleTooLong
    | otherwise = Right (Title normalized)
  where
    normalized = Text.intercalate " " . filter (not . Text.null) $ Text.split isWhitespace input

-- Data.Char.isSpace excludes these three Unicode White_Space characters.
isWhitespace :: Char -> Bool
isWhitespace c = isSpace c || c `elem` ['\x85', '\x2028', '\x2029']

renderTitle :: Title -> Text
renderTitle (Title title) = title

data ArticleState = Unread | Read
    deriving stock (Eq, Show)

data Article = Article
    { articleId :: ArticleId
    , articleTitle :: Title
    , articleUrl :: WebUrl
    , articleState :: ArticleState
    , articleCreatedAt :: UTCTime
    }
    deriving stock (Eq)

newArticle :: ArticleId -> Title -> WebUrl -> UTCTime -> Article
newArticle identifier title url = Article identifier title url Unread

rename :: Title -> Article -> Article
rename title article = article{articleTitle = title}

setReadState :: ArticleState -> Article -> Article
setReadState state article = article{articleState = state}
