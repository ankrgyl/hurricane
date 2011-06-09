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

{-# LANGUAGE ScopedTypeVariables #-}
module Hurricane.Application
(
  runApplication
) where

import qualified Hurricane.Web as Web
import Hurricane.StaticHandler (staticHandler)

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
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP

import qualified Hurricane.Internal.Routes as R
import Hurricane.Internal.Util (unimplemented, Fail(..))

data HandlerWrapper = Static | Dynamic Web.Handler deriving (Show)

runApplication :: Web.ApplicationOptions -> Web.RouteSpec -> IO ()

runApplication opt spec = do
  -- Initialization Steps. May throw exception if something is broken, before we
  -- start serving traffic.
  routeTree <- case installRoutes opt spec of
                (Left e) -> throw e
                (Right tree) -> return tree
  let handler = hurricaneWrapper opt routeTree
  Warp.run (Web.port_num opt) handler

installRoutes :: Web.ApplicationOptions -> Web.RouteSpec -> Either R.InvalidRoutes (R.RouteTree HandlerWrapper)

installRoutes opt handlers =
  let 
    handlers' = [(s, Dynamic h, spec) | (s, h, spec) <- handlers]
    withStatic = (TE.encodeUtf8 $ Web.static_url_prefix opt, Static, R.Prefix) : handlers'
  in 
    R.buildRouteTree withStatic

hurricaneWrapper :: Web.ApplicationOptions -> (R.RouteTree HandlerWrapper) -> Wai.Request -> Web.FinalResponse
hurricaneWrapper opt routes request =
  EL.consume >>= \rawBody ->
    liftIO $ do
      let (args, handler) = R.matchRoute routes (Wai.pathInfo request)
      result :: Wai.Response <- 
        case handler of 
          R.FailMatch -> 
            throw $ Fail "no match found"
          R.PrefixMatch Static suffix -> 
            staticHandler opt request suffix
          _ ->
            throw unimplemented
      return result
