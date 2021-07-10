{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Feature.InsertSpec where

import           SpecHelper

import           Network.Wai.Test          (SResponse (simpleBody, simpleHeaders, simpleStatus))
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import qualified Data.Aeson                as JSON
import           Data.Maybe                (fromJust)


import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status

import           TestTypes                 (IncPK, incNullableStr, incStr)

spec :: Spec
spec = with appWithFixture' $
  describe "Posting new record" $ do
    it "accepts disparate json types" $
      post "/menagerie"
        [json| {
          "integer":13,"double":3.14159,"varchar":"testing!"
        , "boolean":false,"date":"01/01/1990","money":"$3.99"
        } |]
        `shouldRespondWith` 201

    context "with no pk supplied" $ do
      context "into a table with auto-incrementing pk" $
        it "succeeds with 201 and link" $ do
          p <- post "/auto_incrementing_pk" [json| { "non_nullable_string":"not null" } |]
          liftIO $ do
            simpleBody p `shouldBe` ""
            simpleHeaders p `shouldSatisfy` matchHeader hLocation "/auto_incrementing_pk\\?id=eq\\.[0-9]+"
            simpleStatus p `shouldBe` created201
          let Just location = getHeader hLocation $ simpleHeaders p
          r <- get location
          let [record] = fromJust (JSON.decode $ simpleBody r :: Maybe [IncPK])
          liftIO $ do
            incStr record `shouldBe` "not null"
            incNullableStr record `shouldBe` Nothing

      context "into a table with simple pk" $
        it "fails with 400 and error" $
          post "/simple_pk" [json| { "extra":"foo"} |]
            `shouldRespondWith` 400

    context "with compound pk supplied" $
      it "builds response location header appropriately" $
        post "/compound_pk" [json| { "k1":12, "k2":42 } |]
          `shouldRespondWith` 201 {
            matchHeaders = [
              "Location" <:> "/compound_pk?k1=eq.12&k2=eq.42"
            ]
          }
