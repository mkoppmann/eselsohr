module Lib.Ui.Web.Page.UnlockCollection
  ( handler
  ) where

import qualified Lib.Ui.Web.Page.Layout                              as Layout
import qualified Lib.Ui.Web.Page.Static                              as Static

import           Lib.Ui.Web.Route                                     ( HtmlPage )

handler :: (Monad m) => m HtmlPage
handler = Layout.renderM Static.unlockCollection
