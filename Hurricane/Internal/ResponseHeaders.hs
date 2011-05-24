module Hurricane.Internal.ResponseHeaders
(
  ResponseHeaders(..)
) where

import qualified Data.ByteString as B

data ResponseHeaders = ResponseHeaders {
  acceptRanges :: Maybe B.ByteString,
  age :: Maybe B.ByteString,
  allow :: Maybe B.ByteString,
  cacheControl :: Maybe B.ByteString,
  connection :: Maybe B.ByteString,
  contentEncoding :: Maybe B.ByteString,
  contentLanguage :: Maybe B.ByteString,
  contentLength :: Maybe B.ByteString,
  contentLocation :: Maybe B.ByteString,
  contentMD5 :: Maybe B.ByteString,
  contentDisposition :: Maybe B.ByteString,
  contentRange :: Maybe B.ByteString,
  contentType :: Maybe B.ByteString,
  date :: Maybe B.ByteString,
  eTag :: Maybe B.ByteString,
  expires :: Maybe B.ByteString,
  lastModified :: Maybe B.ByteString,
  link :: Maybe B.ByteString,
  location :: Maybe B.ByteString,
  p3P :: Maybe B.ByteString,
  pragma :: Maybe B.ByteString,
  proxyAuthenticate :: Maybe B.ByteString,
  refresh :: Maybe B.ByteString,
  retryAfter :: Maybe B.ByteString,
  server :: Maybe B.ByteString,
  setCookie :: Maybe B.ByteString,
  trailer :: Maybe B.ByteString,
  transferEncoding :: Maybe B.ByteString,
  vary :: Maybe B.ByteString,
  via :: Maybe B.ByteString,
  warning :: Maybe B.ByteString,
  wwwAuthenticate :: Maybe B.ByteString
}
