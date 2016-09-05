{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Request (
  HasRequests(..),
  requests,

  Request(..),
  dumpRequest,
  ) where

import           Data.Proxy
import           Data.String.Conversions
import           GHC.TypeLits
import           Servant.API

import           Servant.Request.Internal


-- * HasRequests class

class HasRequests api where
  type Requests api :: *

  mkRequests :: Proxy api -> (forall a . Request a -> Request a)
    -> Requests api

requests :: HasRequests api => Proxy api -> Requests api
requests proxy = mkRequests proxy id

instance (HasRequests a, HasRequests b) =>
  HasRequests (a :<|> b) where

  type Requests (a :<|> b) = Requests a :<|> Requests b
  mkRequests Proxy f =
    mkRequests (Proxy :: Proxy a) f :<|>
    mkRequests (Proxy :: Proxy b) f

instance ReflectMethod method =>
  HasRequests (Verb (method :: StdMethod) statusCode contentTypes result) where

  type Requests (Verb method statusCode contentTypes result) = Request result
  mkRequests Proxy f = f $ emptyRequest{
    method = reflectMethod (Proxy :: Proxy method)
  }

instance forall api path . (KnownSymbol path, HasRequests api) =>
  HasRequests ((path :: Symbol) :> api) where

  type Requests (path :> api) = Requests api
  mkRequests Proxy f =
    mkRequests (Proxy :: Proxy api)
      (appendPath (cs path) . f)
    where
      path = symbolVal (Proxy :: Proxy path)

instance (KnownSymbol name, ToHttpApiData param, HasRequests api) =>
  HasRequests (QueryParam name param :> api) where

  type Requests (QueryParam name param :> api) =
    Maybe param -> Requests api
  mkRequests Proxy f param =
    mkRequests api
      (maybe id (addQueryParam name) param . f)
    where
      api :: Proxy api
      api = Proxy

      name = cs $ symbolVal (Proxy :: Proxy name)

instance (ToHttpApiData c, HasRequests api) =>
  HasRequests (Capture name c :> api) where

  type Requests (Capture name c :> api) = c -> Requests api
  mkRequests Proxy f captureValue =
    mkRequests (Proxy :: Proxy api)
      (appendPath (cs $ toUrlPiece captureValue) . f)
