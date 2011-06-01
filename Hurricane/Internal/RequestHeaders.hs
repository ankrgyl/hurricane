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

module Hurricane.Internal.RequestHeaders
(
  RequestHeaders(..),
  acceptHeaderName,
  acceptCharsetHeaderName,
  acceptEncodingHeaderName,
  acceptLanguageHeaderName,
  authorizationHeaderName,
  cachecontrolHeaderName,
  connectionHeaderName,
  cookieHeaderName,
  contentLengthHeaderName,
  contentMD5HeaderName,
  contentTypeHeaderName,
  dateHeaderName,
  expectHeaderName,
  fromHeaderName,
  hostHeaderName,
  ifMatchHeaderName,
  ifModifiedSinceHeaderName,
  ifNoneMatchHeaderName,
  ifRangeHeaderName,
  ifUnmodifiedSinceHeaderName,
  maxForwardsHeaderName,
  pragmaHeaderName,
  proxyAuthorizationHeaderName,
  rangeHeaderName,
  refererHeaderName,
  tEHeaderName,
  upgradeHeaderName,
  userAgentHeaderName,
  viaHeaderName,
  warningHeaderName
) where

import qualified Data.Text as T

{-- Headers that we don't actively used are just
 -- ByteStrings for now --}
data RequestHeaders = RequestHeaders {
  accept :: Maybe T.Text,
  acceptCharset :: Maybe T.Text,
  acceptEncoding :: Maybe T.Text,
  acceptLanguage :: Maybe T.Text,
  authorization :: Maybe T.Text,
  cachecontrol :: Maybe T.Text,
  connection :: Maybe T.Text,
  cookie :: Maybe T.Text,
  contentLength :: Maybe T.Text,
  contentMD5 :: Maybe T.Text,
  contentType :: Maybe T.Text,
  date :: Maybe T.Text,
  expect :: Maybe T.Text,
  from :: Maybe T.Text,
  host :: Maybe T.Text,
  ifMatch :: Maybe T.Text,
  ifModifiedSince :: Maybe T.Text,
  ifNoneMatch :: Maybe T.Text,
  ifRange :: Maybe T.Text,
  ifUnmodifiedSince :: Maybe T.Text,
  maxForwards :: Maybe T.Text,
  pragma :: Maybe T.Text,
  proxyAuthorization :: Maybe T.Text,
  range :: Maybe T.Text,
  referer :: Maybe T.Text,
  tE :: Maybe T.Text,
  upgrade :: Maybe T.Text,
  userAgent :: Maybe T.Text,
  via :: Maybe T.Text,
  warning :: Maybe T.Text
}

acceptHeaderName :: T.Text
acceptHeaderName = "Accept"

acceptCharsetHeaderName :: T.Text
acceptCharsetHeaderName = "Accept-Charset"

acceptEncodingHeaderName :: T.Text
acceptEncodingHeaderName = "Accept-Encoding"

acceptLanguageHeaderName :: T.Text
acceptLanguageHeaderName = "Accept-Language"

authorizationHeaderName :: T.Text
authorizationHeaderName = "Authorization"

cachecontrolHeaderName :: T.Text
cachecontrolHeaderName = "Cache-Control"

connectionHeaderName :: T.Text
connectionHeaderName = "Connection"

cookieHeaderName :: T.Text
cookieHeaderName = "Cookie"

contentLengthHeaderName :: T.Text
contentLengthHeaderName = "Content-Length"

contentMD5HeaderName :: T.Text
contentMD5HeaderName = "Content-MD5"

contentTypeHeaderName :: T.Text
contentTypeHeaderName = "Content-Type"

dateHeaderName :: T.Text
dateHeaderName = "Date"

expectHeaderName :: T.Text
expectHeaderName = "Expect"

fromHeaderName :: T.Text
fromHeaderName = "From"

hostHeaderName :: T.Text
hostHeaderName = "Host"

ifMatchHeaderName :: T.Text
ifMatchHeaderName = "If-Match"

ifModifiedSinceHeaderName :: T.Text
ifModifiedSinceHeaderName = "If-Modified-Since"

ifNoneMatchHeaderName :: T.Text
ifNoneMatchHeaderName = "If-None-Match"

ifRangeHeaderName :: T.Text
ifRangeHeaderName = "If-Range"

ifUnmodifiedSinceHeaderName :: T.Text
ifUnmodifiedSinceHeaderName = "If-Unmodified-Since"

maxForwardsHeaderName :: T.Text
maxForwardsHeaderName = "Max-Forwards"

pragmaHeaderName :: T.Text
pragmaHeaderName = "Pragma"

proxyAuthorizationHeaderName :: T.Text
proxyAuthorizationHeaderName = "Proxy-Authorization"

rangeHeaderName :: T.Text
rangeHeaderName = "Range"

refererHeaderName :: T.Text
refererHeaderName = "Referer"

tEHeaderName :: T.Text
tEHeaderName = "TE"

upgradeHeaderName :: T.Text
upgradeHeaderName = "Upgrade"

userAgentHeaderName :: T.Text
userAgentHeaderName = "User-Agent"

viaHeaderName :: T.Text
viaHeaderName = "Via"

warningHeaderName :: T.Text
warningHeaderName = "Warning"
