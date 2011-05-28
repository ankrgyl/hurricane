module Hurricane.Web
(
  Handler(..)
) where

import qualified Data.ByteString as B

type Handler = B.ByteString -> B.ByteString
