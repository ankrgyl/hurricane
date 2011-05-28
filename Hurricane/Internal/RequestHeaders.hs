module Hurricane.Internal.RequestHeaders
(
  RequestHeaders(..)
) where

import qualified Data.ByteString as B
import qualified Network.HTTP.Types as H


{-- Headers that we don't actively used are just
 -- ByteStrings for now --}
data RequestHeaders = RequestHeaders {
  accept :: Maybe B.ByteString,
  acceptCharset :: Maybe B.ByteString,
  acceptEncoding :: Maybe B.ByteString,
  acceptLanguage :: Maybe B.ByteString,
  authorization :: Maybe B.ByteString,
  cachecontrol :: Maybe B.ByteString,
  connection :: Maybe B.ByteString,
  cookie :: Maybe B.ByteString,
  contentLength :: Maybe B.ByteString,
  contentMD5 :: Maybe B.ByteString,
  contentType :: Maybe B.ByteString,
  date :: Maybe B.ByteString,
  expect :: Maybe B.ByteString,
  from :: Maybe B.ByteString,
  host :: Maybe B.ByteString,
  ifMatch :: Maybe B.ByteString,
  ifModifiedSince :: Maybe B.ByteString,
  ifNoneMatch :: Maybe B.ByteString,
  ifRange :: Maybe B.ByteString,
  ifUnmodifiedSince :: Maybe B.ByteString,
  maxForwards :: Maybe B.ByteString,
  pragma :: Maybe B.ByteString,
  proxyAuthorization :: Maybe B.ByteString,
  range :: Maybe B.ByteString,
  referer :: Maybe B.ByteString,
  tE :: Maybe B.ByteString,
  upgrade :: Maybe B.ByteString,
  userAgent :: Maybe B.ByteString,
  via :: Maybe B.ByteString,
  warning :: Maybe B.ByteString
}

parseHeaders :: H.RequestHeaders
