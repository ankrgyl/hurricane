{-# LANGUAGE DeriveDataTypeable #-}

module Hurricane.Internal.Util (
  Fail(..)
  intToByteString
) where

import Control.Exception (Exception(..))
import Data.Typeable (Typeable)

data Fail = Fail String
            deriving (Show, Typeable)
instance Exception Fail

import Data.ByteString as B
import Data.Text as T
import Data.Text.Encoding as E

intToByteString :: Int -> B.ByteString
intToByteString n = E.encodeUtf8 $ T.pack $ show $ n
