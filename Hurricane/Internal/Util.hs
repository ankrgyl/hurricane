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

{-# LANGUAGE DeriveDataTypeable #-}

module Hurricane.Internal.Util (
  Fail(..),
  unimplemented,
  intToByteString
) where

import Control.Exception (Exception(..))
import Data.Typeable (Typeable)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data Fail = Fail String
            deriving (Show, Typeable)
instance Exception Fail

unimplemented :: Fail
unimplemented = Fail "unimplemented"

intToByteString :: Int -> B.ByteString
intToByteString n = E.encodeUtf8 $ T.pack $ show $ n
