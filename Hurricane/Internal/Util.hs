{-# LANGUAGE DeriveDataTypeable #-}

module Hurricane.Internal.Util (
  Fail(..),
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

intToByteString :: Int -> B.ByteString
intToByteString n = E.encodeUtf8 $ T.pack $ show $ n
