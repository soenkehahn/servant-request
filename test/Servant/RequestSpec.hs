{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.RequestSpec where

import           Data.Proxy
import           Servant.API
import           Test.Hspec

import           Servant.Request

spec :: Spec
spec = do
  describe "requests" $ do
    describe "allows to inspect the request" $ do
      it "contains query params" $ do
        let r = withQueryParam 42
        queryParams r `shouldBe` [("bar", "42")]

      it "contains the method" $ do
        let r = withQueryParam 42
        method r `shouldBe` "GET"

type Api =
  "foo" :> QueryParam "bar" Int :> Get '[JSON] String

api :: Proxy Api
api = Proxy

withQueryParam :: Requests Api
withQueryParam = requests api
