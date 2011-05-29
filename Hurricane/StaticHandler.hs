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

staticHandler :: FilePath -> Iteratee B.ByteString IO Wai.Response
staticHandler path = return $ Wai.ResponseFile
                  HTTP.status200
                  [("Content-Type", "text/plain")]
                  path Nothing
