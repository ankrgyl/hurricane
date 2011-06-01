module Hurricane.Web
(
  ApplicationOptions(..),
  Handler(..),
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hurricane.StaticHandler (staticHandler)

import qualified Hurricane.Internal.Routes as R

data ApplicationOptions = ApplicationOptions {
                            -- URL Prefix corresponding to static traffic
                            static_url_prefix :: T.Text, 

                            -- File system path 
                            static_path :: T.Text 
                          }

-- XXX Temporary
type HandlerMethod = B.ByteString -> B.ByteString
type Handler = {
      headMethod :: Maybe HandlerMethod
      getMethod :: Maybe HandlerMethod
      postMethod :: Maybe HandlerMethod
      deleteMethod :: Maybe HandlerMethod
      putMethod :: Maybe HandlerMethod
    }

data HandlerWrapper = Static | Dynamic Handler

installRoutes :: ApplicationOptions -> [(B.ByteString, Handler)] -> R.RouteTree HandlerWrapper
installRoutes opt handlers =
  let 
    handlers' = [(s, Dynamic h) | (s, h) <- handlers]
    withStatic = (TE.encodeUtf8 $ static_url_prefix opt, Static) : handlers'
  in 
    R.buildRouteTree withStatic
