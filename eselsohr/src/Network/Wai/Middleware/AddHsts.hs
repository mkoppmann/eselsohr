module Network.Wai.Middleware.AddHsts
  ( addHsts,
  )
where

import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Request (appearsSecure)

-- | Add the @HTTP Strict Transport Security@ header to secure requests.
addHsts :: Middleware
addHsts app req send =
  if appearsSecure req
    then addHeaders [("Strict-Transport-Security", "max-age=31536000")] app req send
    else app req send
