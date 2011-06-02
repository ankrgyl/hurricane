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

import qualified Network.Wai as Wai
import qualified Data.ByteString as B
import qualified Network.HTTP.Types as HTTP

import Control.Exception (throw)
import Data.Enumerator (Iteratee)
import System.IO (FilePath)

import Hurricane.Internal.Util (unimplemented)

{-- <File Path> is the path to the file to serve. --}
staticHandler :: FilePath -> Iteratee B.ByteString IO Wai.Response
staticHandler path = return $ Wai.ResponseFile
                  HTTP.status200
                  [("Content-Type", "text/plain")]
                  path Nothing
