module Lib.Ui.Web.Controller.Static
  ( static
  , startpage
  , invalidToken
  ) where

import           Clay                           ( Css )

import qualified Lib.Ui.Web.Page.Layout        as Layout
import qualified Lib.Ui.Web.Page.Static        as Static
import qualified Lib.Ui.Web.Page.Style         as Style
import qualified Lib.Ui.Web.Route              as Route

import           Lib.Ui.Web.Route               ( AppServer
                                                , HtmlPage
                                                , StaticSite
                                                )

static :: StaticSite AppServer
static = Route.StaticSite { Route.startpage    = startpage
                          , Route.invalidToken = invalidToken
                          , Route.stylesheet   = stylesheet
                          }

startpage :: (Monad m) => m HtmlPage
startpage = Layout.renderM Static.startPage

invalidToken :: (Monad m) => m HtmlPage
invalidToken = Layout.renderM Static.invalidToken

stylesheet :: (Monad m) => m Css
stylesheet = pure Style.app
