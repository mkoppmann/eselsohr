module Network.Wai.Middleware.NoOp
  ( noOp,
  )
where

import Network.Wai (Middleware)

-- | Do nothing with the request or response.
-- Useful in pattern matching cases, when you have to provide a middleware.
noOp :: Middleware
noOp app = app
