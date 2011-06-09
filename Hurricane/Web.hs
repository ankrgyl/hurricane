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
{-# LANGUAGE TypeSynonymInstances #-}
module Hurricane.Web
(
  ApplicationOptions(..),
  Handler(..),
  RouteSpec(..),
  HandlerError(..),
  HandlerResponse(..),
  FinalResponse(..),
) where

import Control.Monad.Error (Error(..), ErrorT(..))
import Control.Monad.Trans (MonadIO(..), liftIO)
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

data ApplicationOptions = ApplicationOptions {
                            -- URL Prefix corresponding to static traffic
                            static_url_prefix :: T.Text, 

                            -- File system path 
                            static_path :: T.Text,

                            port_num :: Int
                          }
                          deriving (Show, Eq)

data HandlerError = HandlerError HTTP.Status deriving (Show, Eq)
instance Error HandlerError
type HandlerMonad = (ErrorT HandlerError IO) 
type HandlerResponse = HandlerMonad Wai.Response

-- XXX Temporary
--type HandlerMethod = B.ByteString -> B.ByteString
type HandlerMethod = ()

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
               } deriving (Show)
