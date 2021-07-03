{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Feature.RangeSpec where

import           SpecHelper

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec :: Spec
spec = with appWithFixture' $ do
  describe "GET /" $ do
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

    it "lists views in schema" $
      get "/" `shouldRespondWith` [json|
        [{"schema":"public","name":"auto_incrementing_pk","insertable":true}]
      |]

  describe "GET /view" $
    context "without range headers" $
      context "with response under server size limit" $
        it "returns whole range with status 206" $
          get "/auto_incrementing_pk" `shouldRespondWith` 206

