{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Request (
  HasRequests(..),
  requests,

  Request(..),
  ) where

import           Data.Proxy
import           Data.String.Conversions
import           GHC.TypeLits
import           Servant.API

import           Servant.Request.Internal


-- * HasRequests class

class HasRequests api where
  type Requests api :: *

  mkRequests :: Proxy api -> (Request -> Request) -> Requests api

requests :: HasRequests api => Proxy api -> Requests api
requests proxy = mkRequests proxy id

instance ReflectMethod method =>
  HasRequests (Verb (method :: StdMethod) statusCode contentTypes result) where

  type Requests (Verb method statusCode contentTypes result) = Request
  mkRequests Proxy f = f $ emptyRequest{
    method = reflectMethod (Proxy :: Proxy method)
  }

instance forall api path . (HasRequests api) =>
  HasRequests ((path :: Symbol) :> api) where

  type Requests (path :> api) = Requests api
  mkRequests Proxy = mkRequests (Proxy :: Proxy api)

instance (KnownSymbol name, ToHttpApiData param, HasRequests api) =>
  HasRequests (QueryParam name param :> api) where

  type Requests (QueryParam name param :> api) =
    param -> Requests api
  mkRequests Proxy f param =
    mkRequests api
      (addQueryParam name param . f)
    where
      api :: Proxy api
      api = Proxy

      name = cs $ symbolVal (Proxy :: Proxy name)
