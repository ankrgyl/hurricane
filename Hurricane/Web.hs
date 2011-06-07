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

{-# LANGUAGE OverloadedStrings #-}
module Hurricane.Web
(
  ApplicationOptions(..),
  Handler,
  RouteSpec,
  FinalResponse,
  makeApplication
) where

import Control.Monad.Error (Error(..))
import Control.Monad.Trans (liftIO)
import Control.Exception (throw)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Enumerator (Iteratee)
import qualified Data.Enumerator.List as EL
import Blaze.ByteString.Builder (fromByteString)

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

import qualified Hurricane.Internal.Routes as R
import Hurricane.Internal.Util (unimplemented)

data ApplicationOptions = ApplicationOptions {
                            -- URL Prefix corresponding to static traffic
                            static_url_prefix :: T.Text, 

                            -- File system path 
                            static_path :: T.Text 
                          }


-- XXX Temporary
type HandlerMethod = B.ByteString -> B.ByteString

type RouteSpec = [(B.ByteString, Handler, R.MatchSpec)]

type FinalResponse = Iteratee B.ByteString IO Wai.Response

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

installRoutes :: ApplicationOptions -> RouteSpec -> Either R.InvalidRoutes (R.RouteTree HandlerWrapper)
installRoutes opt handlers =
  let 
    handlers' = [(s, Dynamic h, spec) | (s, h, spec) <- handlers]
    withStatic = (TE.encodeUtf8 $ static_url_prefix opt, Static, R.Prefix) : handlers'
  in 
    R.buildRouteTree withStatic


hurricaneWrapper :: ApplicationOptions -> (R.RouteTree HandlerWrapper) -> Wai.Request -> (IO Wai.Response)
hurricaneWrapper opt routes request = do
  let handler = R.matchRoute routes (Wai.pathInfo request)
  putStrLn "hey, we made it!"
  throw unimplemented
  

makeApplication :: ApplicationOptions -> RouteSpec -> Wai.Application
makeApplication opt spec request = 
  EL.consume >>= \rawBody ->
  liftIO $ do
    let body = TE.decodeUtf8 (B.concat rawBody)
    let routeTree = case installRoutes opt spec of
                      (Left _) -> throw unimplemented
                      (Right tree) -> tree
    handlerResponse <- hurricaneWrapper opt routeTree request
    return $ handlerResponse
