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
module Hurricane.StaticHandler
(
  staticHandler
) where

import Data.ByteString as B
import qualified Data.Text as T

import Control.Monad.Trans (liftIO)
import Data.Enumerator (Iteratee)
import Blaze.ByteString.Builder (fromByteString)

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Application.Static as S

import qualified Hurricane.Web as Web

import Control.Exception (throw)
import Hurricane.Internal.Util (unimplemented)

type FinalResponse = Iteratee B.ByteString IO Wai.Response

staticHandler :: Web.ApplicationOptions -> Wai.Request -> FinalResponse
staticHandler = throw unimplemented

{-- staticHandler_get <Application Options> <Request> <Include Body> -> <Response> --}
staticHandler_get :: Web.ApplicationOptions -> Wai.Request -> Bool -> FinalResponse

staticHandler_get opt req includeBody = liftIO $ do
  return $ Wai.ResponseBuilder
    HTTP.status200
    []
    $ fromByteString "TO BE IMPLEMENTED"
