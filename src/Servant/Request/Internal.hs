{-# LANGUAGE OverloadedStrings #-}

module Servant.Request.Internal where

import           Data.ByteString hiding (map)
import           Data.Monoid
import           Data.String.Conversions
import           Data.Text hiding (map)
import           Web.HttpApiData

data Request
  = Request {
    method :: ByteString,
    path :: [ByteString],
    queryParams :: [(ByteString, Text)]
  }

emptyRequest :: Request
emptyRequest = Request {
  method = error "method not initialized (fixme?)",
  path = [],
  queryParams = []
}

appendPath :: ByteString -> Request -> Request
appendPath segment request = request{
  path = path request ++ [segment]
}

renderPath :: [ByteString] -> ByteString
renderPath segments =
  "/" <> Data.ByteString.intercalate "/" segments

addQueryParam :: ToHttpApiData a => ByteString -> a -> Request -> Request
addQueryParam name param request = request{
  queryParams = (name, toQueryParam param) : queryParams request
}

renderQueryParams :: [(ByteString, Text)] -> ByteString
renderQueryParams params =
  "?" <> Data.ByteString.intercalate "&" (map inner params)
  where
    inner (key, value) = key <> "=" <> cs value

dumpRequest :: Request -> Text
dumpRequest (Request method path queryParams) =
  cs method
  <> " " <> cs (renderPath path)
  <> cs (renderQueryParams queryParams)
