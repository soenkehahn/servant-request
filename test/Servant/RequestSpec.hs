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
      it "contains the method" $ do
        let r = withQueryParam Nothing
        method r `shouldBe` "GET"

      it "contains the path" $ do
        let r = withQueryParam Nothing
        path r `shouldBe` ["foo"]

      it "contains query params" $ do
        let r = withQueryParam $ Just 42
        queryParams r `shouldBe` [("bar", "42")]

  describe "dumpRequest" $ do
    it "looks like a http dump" $ do
      let r = withQueryParam $ Just 42
      dumpRequest r `shouldBe`
        "GET /foo?bar=42"

type Api =
  "foo" :> QueryParam "bar" Int :> Get '[JSON] String :<|>
  "bar" :> Get '[JSON] String

api :: Proxy Api
api = Proxy

withQueryParam :: Maybe Int -> Request String
withQueryParam :<|> _ = requests api
