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

{-# Language OverloadedStrings #-}

module Hurricane.Internal.ResponseHeaders
(
  ResponseHeaders(..),
  acceptRangesHeaderName,
  ageHeaderName,
  allowHeaderName,
  cacheControlHeaderName,
  connectionHeaderName,
  contentEncodingHeaderName,
  contentLanguageHeaderName,
  contentLengthHeaderName,
  contentLocationHeaderName,
  contentMD5HeaderName,
  contentDispositionHeaderName,
  contentRangeHeaderName,
  contentTypeHeaderName,
  dateHeaderName,
  eTagHeaderName,
  expiresHeaderName,
  lastModifiedHeaderName,
  linkHeaderName,
  locationHeaderName,
  p3PHeaderName,
  pragmaHeaderName,
  proxyAuthenticateHeaderName,
  refreshHeaderName,
  retryAfterHeaderName,
  serverHeaderName,
  setCookieHeaderName,
  trailerHeaderName,
  transferEncodingHeaderName,
  varyHeaderName,
  viaHeaderName,
  warningHeaderName,
  wwwAuthenticateHeaderName
) where

import qualified Data.Text as T

data ResponseHeaders = ResponseHeaders {
  acceptRanges :: Maybe T.Text,
  age :: Maybe T.Text,
  allow :: Maybe T.Text,
  cacheControl :: Maybe T.Text,
  connection :: Maybe T.Text,
  contentEncoding :: Maybe T.Text,
  contentLanguage :: Maybe T.Text,
  contentLength :: Maybe T.Text,
  contentLocation :: Maybe T.Text,
  contentMD5 :: Maybe T.Text,
  contentDisposition :: Maybe T.Text,
  contentRange :: Maybe T.Text,
  contentType :: Maybe T.Text,
  date :: Maybe T.Text,
  eTag :: Maybe T.Text,
  expires :: Maybe T.Text,
  lastModified :: Maybe T.Text,
  link :: Maybe T.Text,
  location :: Maybe T.Text,
  p3P :: Maybe T.Text,
  pragma :: Maybe T.Text,
  proxyAuthenticate :: Maybe T.Text,
  refresh :: Maybe T.Text,
  retryAfter :: Maybe T.Text,
  server :: Maybe T.Text,
  setCookie :: Maybe T.Text,
  trailer :: Maybe T.Text,
  transferEncoding :: Maybe T.Text,
  vary :: Maybe T.Text,
  via :: Maybe T.Text,
  warning :: Maybe T.Text,
  wwwAuthenticate :: Maybe T.Text
}

acceptRangesHeaderName :: T.Text
acceptRangesHeaderName = "Accept-Ranges"

ageHeaderName :: T.Text
ageHeaderName = "Age"

allowHeaderName :: T.Text
allowHeaderName = "Allow"

cacheControlHeaderName :: T.Text
cacheControlHeaderName = "Cache-Control"

connectionHeaderName :: T.Text
connectionHeaderName = "Connection"

contentEncodingHeaderName :: T.Text
contentEncodingHeaderName = "Content-Encoding"

contentLanguageHeaderName :: T.Text
contentLanguageHeaderName = "Content-Language"

contentLengthHeaderName :: T.Text
contentLengthHeaderName = "Content-Length"

contentLocationHeaderName :: T.Text
contentLocationHeaderName = "Content-Location"

contentMD5HeaderName :: T.Text
contentMD5HeaderName = "Content-MD5"

contentDispositionHeaderName :: T.Text
contentDispositionHeaderName = "Content-Disposition"

contentRangeHeaderName :: T.Text
contentRangeHeaderName = "Content-Range"

contentTypeHeaderName :: T.Text
contentTypeHeaderName = "Content-Type"

dateHeaderName :: T.Text
dateHeaderName = "Date"

eTagHeaderName :: T.Text
eTagHeaderName = "ETag"

expiresHeaderName :: T.Text
expiresHeaderName = "Expires"

lastModifiedHeaderName :: T.Text
lastModifiedHeaderName = "Last-Modified"

linkHeaderName :: T.Text
linkHeaderName = "Link"

locationHeaderName :: T.Text
locationHeaderName = "Location"

p3PHeaderName :: T.Text
p3PHeaderName = "P3P"

pragmaHeaderName :: T.Text
pragmaHeaderName = "Pragma"

proxyAuthenticateHeaderName :: T.Text
proxyAuthenticateHeaderName = "Proxy-Authenticate"

refreshHeaderName :: T.Text
refreshHeaderName = "Refresh"

retryAfterHeaderName :: T.Text
retryAfterHeaderName = "Retry-After"

serverHeaderName :: T.Text
serverHeaderName = "Server"

setCookieHeaderName :: T.Text
setCookieHeaderName = "Set-Cookie"

trailerHeaderName :: T.Text
trailerHeaderName = "Trailer"

transferEncodingHeaderName :: T.Text
transferEncodingHeaderName = "Transfer-Encoding"

varyHeaderName :: T.Text
varyHeaderName = "Vary"

viaHeaderName :: T.Text
viaHeaderName = "Via"

warningHeaderName :: T.Text
warningHeaderName = "Warning"

wwwAuthenticateHeaderName :: T.Text
wwwAuthenticateHeaderName = "WWW-Authenticate"
