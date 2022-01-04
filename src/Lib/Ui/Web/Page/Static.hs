module Lib.Ui.Web.Page.Static
  ( startPage
  , invalidToken
  , notAuthorized
  ) where

import           Lucid

import           Lib.Ui.Web.Page.Shared         ( createCollectionForm )

startPage :: Html ()
startPage = do
  h1_ "Welcome to Eselsohr"
  p_
    "Eselsohr is a service focused on simplicity.\
    \ Save web articles and consume them later.\
    \ Start your collection by clicking on the button."
  createCollectionForm

invalidToken :: Html ()
invalidToken = do
  h1_ "Invalid token"
  p_
    "The token you’ve provided was not accepted. This can have multiple\
    \ reasons:"
  ul_ $ do
    li_ "The token was not created by this system"
    li_ "The token expired"
  p_ "You have to provide a valid token to perform this action."

notAuthorized :: Html ()
notAuthorized = do
  h1_ "Not authorized"
  p_ "You must provide an accesstoken to access this page."
