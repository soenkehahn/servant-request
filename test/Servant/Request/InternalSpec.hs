{-# LANGUAGE OverloadedStrings #-}

module Servant.Request.InternalSpec where

import           Test.Hspec

import           Servant.Request.Internal

spec :: Spec
spec = do
  describe "addQueryParam" $ do
    it "adds a query param" $ do
      queryParams (addQueryParam "foo" (42 :: Int) emptyRequest) `shouldBe`
        [("foo", "42")]

    it "adds query params at the beginning" $ do
      let a = addQueryParam "foo" (42 :: Int) emptyRequest
          b = addQueryParam "bar" (23 :: Int) a
      queryParams b `shouldBe` [("bar", "23"), ("foo", "42")]
