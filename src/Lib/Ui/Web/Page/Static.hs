module Lib.Ui.Web.Page.Static
  ( startPage
  , unlockCollection
  , invalidToken
  , notAuthorized
  ) where

import           Lucid
import           Lucid.Servant                                        ( linkAbsHref_ )
import           Servant                                              ( fieldLink )

import qualified Lib.Ui.Web.Route                                    as Route

import           Lib.Ui.Web.Page.Shared                               ( createCollectionForm
                                                                      , unlockCollectionForm
                                                                      )

startPage :: Bool -> Html ()
startPage publicCollectionCreation = do
  h1_ "Welcome to Eselsohr"
  p_ "Eselsohr is a service focused on simplicity.\
    \ Save web articles and consume them later."

  p_ $ do
    "Open an existing collection: "
    a_ [linkAbsHref_ $ fieldLink Route.unlockCollection] "Unlock collection"

  when publicCollectionCreation $ do
    p_ "Create a new collection:"
    createCollectionForm

unlockCollection :: Html ()
unlockCollection = do
  h1_ "Unlock collection"
  p_ "Please provide the accesstoken for your collection."
  unlockCollectionForm

invalidToken :: Html ()
invalidToken = do
  h1_ "Invalid token"
  p_ "The token you’ve provided was not accepted. This can have multiple\
    \ reasons:"
  ul_ $ do
    li_ "The token was not created by this system"
    li_ "The token expired"
  p_ "You have to provide a valid token to perform this action."

notAuthorized :: Html ()
notAuthorized = do
  h1_ "Not authorized"
  p_ "You must provide an accesstoken to access this page."
