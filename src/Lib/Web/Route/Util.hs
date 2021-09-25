module Lib.Web.Route.Util
  ( linkAsText
  ) where

import           Servant                        ( Link
                                                , linkURI
                                                )

linkAsText :: Link -> Text
linkAsText = (<>) "/" . show @Text . linkURI
