
module Servant.Request.Internal where

import           Data.ByteString
import           Data.Text
import           Web.HttpApiData

data Request
  = Request {
    method :: ByteString,
    queryParams :: [(ByteString, Text)]
  }

emptyRequest :: Request
emptyRequest = Request {
  queryParams = [],
  method = error "method not initialized (fixme?)"
}

addQueryParam :: ToHttpApiData a => ByteString -> a -> Request -> Request
addQueryParam name param request = request{
  queryParams = (name, toQueryParam param) : queryParams request
}
