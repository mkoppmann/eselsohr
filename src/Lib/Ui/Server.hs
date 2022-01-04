module Lib.Ui.Server
  ( Api
  , application
  ) where

import qualified Network.Wai.Middleware.EnforceHTTPS                 as EnforceHTTPS

import           Network.Wai                                          ( Middleware )
import           Network.Wai.Handler.Warp                             ( Port )
import           Network.Wai.Middleware.AddHeaders                    ( addHeaders )
import           Network.Wai.Middleware.AddHsts                       ( addHsts )
import           Network.Wai.Middleware.Gzip                          ( def
                                                                      , gzip
                                                                      )
import           Network.Wai.Middleware.MethodOverridePost            ( methodOverridePost )
import           Network.Wai.Middleware.NoOp                          ( noOp )
import           Network.Wai.Middleware.RealIp                        ( realIpHeader )
import           Servant                                              ( (:<|>)(..)
                                                                      , Application
                                                                      , Server
                                                                      , hoistServer
                                                                      , serve
                                                                      )
import           Servant.API.Generic                                  ( toServant )

import qualified Lib.App.Env                                         as Env
import qualified Lib.Ui.Web.Controller.ArticleList                   as Controller
import qualified Lib.Ui.Web.Controller.Collection                    as Controller
import qualified Lib.Ui.Web.Controller.Static                        as Controller

import           Lib.Infra.Log                                        ( runAppAsHandler )
import           Lib.Infra.Monad                                      ( AppEnv )
import           Lib.Ui.Web.Route                                     ( Api )

server :: AppEnv -> Server Api
server env =
  hoistServer (Proxy @Api) (runAppAsHandler env)
    $    toServant Controller.collection
    :<|> toServant Controller.articleList
    :<|> toServant Controller.static

application :: Port -> AppEnv -> Application
application port env@Env.Env {..} =
  -- Response middlewares
  gzip def
    . hstsHeader
    . realIpHeader "X-Forwarded-For"
    . addSecurityHeaders
    . disableCache
    -- Request middlewares
    . enforceHttps
    . methodOverridePost
    $ serve (Proxy @Api) (server env)
 where
  enforceHttps :: Middleware
  enforceHttps = case https of
    Env.HttpsOn  -> EnforceHTTPS.withConfig $ EnforceHTTPS.defaultConfig { EnforceHTTPS.httpsPort = port }
    Env.HttpsOff -> noOp

  hstsHeader :: Middleware
  hstsHeader = case hsts of
    Env.HstsOn  -> addHsts
    Env.HstsOff -> noOp

  disableCache :: Middleware
  disableCache = addHeaders [("Cache-Control", "no-store, must-revalidate, max-age=0")]

  addSecurityHeaders :: Middleware
  addSecurityHeaders = addHeaders
    [ ("Referrer-Policy", "no-referrer")
    , ("X-Content-Type-Options", "nosniff")
    , ( "Content-Security-Policy"
      , "default-src 'none';\
        \ style-src 'self';\
        \ img-src 'self';\
        \ form-action 'self';\
        \ upgrade-insecure-requests;"
      )
    ]
