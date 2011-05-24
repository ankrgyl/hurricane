module Hurricane.Internal.Util (
  intToByteString
) where

import Data.ByteString as B
import Data.Text as T
import Data.Text.Encoding as E

intToByteString :: Int -> B.ByteString
intToByteString n = E.encodeUtf8 $ T.pack $ show $ n
