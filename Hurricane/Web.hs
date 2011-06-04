-- Copyright 2011 Ankur Goyal and other github contributors

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Hurricane.Web
(
  ApplicationOptions(..),
  Handler(..),
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Hurricane.Internal.Routes as R

data ApplicationOptions = ApplicationOptions {
                            -- URL Prefix corresponding to static traffic
                            static_url_prefix :: T.Text, 

                            -- File system path 
                            static_path :: T.Text 
                          }

-- XXX Temporary
type HandlerMethod = B.ByteString -> B.ByteString

{-- A handler is a piece of data that responds to some subset of
 -- HEAD, GET, POST, DELETE, and PUT requests. 
 --}
data Handler = Handler {
                 headMethod :: Maybe HandlerMethod,
                 getMethod :: Maybe HandlerMethod,
                 postMethod :: Maybe HandlerMethod,
                 deleteMethod :: Maybe HandlerMethod,
                 putMethod :: Maybe HandlerMethod
               }

data HandlerWrapper = Static | Dynamic Handler

installRoutes :: ApplicationOptions -> [(B.ByteString, Handler, R.MatchSpec)] -> Either R.InvalidRoutes (R.RouteTree HandlerWrapper)
installRoutes opt handlers =
  let 
    handlers' = [(s, Dynamic h, spec) | (s, h, spec) <- handlers]
    withStatic = (TE.encodeUtf8 $ static_url_prefix opt, Static, R.Prefix) : handlers'
  in 
    R.buildRouteTree withStatic
