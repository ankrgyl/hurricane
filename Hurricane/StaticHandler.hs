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

import qualified Data.ByteString as B
import qualified Data.Text as T

import Control.Monad.Trans (liftIO)
import Data.Enumerator (Iteratee)
import Blaze.ByteString.Builder (fromByteString)

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Application.Static as S

import qualified Hurricane.Web as Web

--import Control.Exception (throw)
--import Hurricane.Internal.Util (unimplemented)

import System.FilePath ((</>))
import System.Directory (doesFileExist)


staticHandler :: Web.ApplicationOptions -> Wai.Request -> [T.Text] -> IO Wai.Response
staticHandler opt req pieces = staticHandler_get opt req pieces True

{-- staticHandler_get <Application Options> <Request> <Include Body> -> <Response> --}
staticHandler_get :: Web.ApplicationOptions -> Wai.Request -> [T.Text] -> Bool -> IO Wai.Response

staticHandler_get opt _ relPieces _ = do
  let fullPath = pathFromPieces (Web.static_path opt) relPieces
  putStrLn fullPath
  --side effects!
  shouldServe <- doesFileExist fullPath 
  return $ case shouldServe of
    False -> Wai.ResponseBuilder HTTP.status404 [] (fromByteString "FAILURE")
    True -> Wai.ResponseFile HTTP.status200 [] fullPath Nothing


{-- pathFromPieces <root> <pieces> --}
pathFromPieces :: T.Text -> [T.Text] -> String
pathFromPieces root pieces = foldl (</>) (T.unpack root) (map T.unpack pieces)

{--
  return $ Wai.ResponseBuilder
    HTTP.status200
    []
    $ fromByteString "TO BE IMPLEMENTED"
--}
