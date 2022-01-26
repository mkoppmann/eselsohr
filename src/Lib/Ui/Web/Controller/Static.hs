module Lib.Ui.Web.Controller.Static
  ( static
  , startpage
  , invalidToken
  ) where

import           Clay                                                 ( Css )

import qualified Lib.App.Env                                         as Env
import qualified Lib.Ui.Web.Page.Layout                              as Layout
import qualified Lib.Ui.Web.Page.Static                              as Static
import qualified Lib.Ui.Web.Page.Style                               as Style
import qualified Lib.Ui.Web.Route                                    as Route

import           Lib.App.Env                                          ( CollectionCreation
                                                                      , Has
                                                                      , grab
                                                                      )
import           Lib.Ui.Web.Route                                     ( AppServer
                                                                      , HtmlPage
                                                                      , StaticSite
                                                                      )

type WithEnv env m = (MonadReader env m, Has CollectionCreation env)

static :: StaticSite AppServer
static =
  Route.StaticSite { Route.startpage = startpage, Route.invalidToken = invalidToken, Route.stylesheet = stylesheet }

startpage :: WithEnv env m => m HtmlPage
startpage = do
  collectionCreation <- grab @CollectionCreation
  case collectionCreation of
    Env.Public  -> Layout.renderM $ Static.startPage True
    Env.Private -> Layout.renderM $ Static.startPage False

invalidToken :: (Monad m) => m HtmlPage
invalidToken = Layout.renderM Static.invalidToken

stylesheet :: (Monad m) => m Css
stylesheet = pure Style.app
