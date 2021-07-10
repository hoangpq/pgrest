{-# LANGUAGE OverloadedStrings #-}

module Feature.RangeSpec where

import           SpecHelper

import           Test.Hspec
import           Test.Hspec.Wai

import           Network.HTTP.Types

import           Network.Wai.Test   (SResponse (simpleHeaders, simpleStatus))

spec :: Spec
spec = with appWithFixture' $
  describe "GET /view" $ do
    context "without range headers" $
      context "with response under server size limit" $
        it "returns whole range with status 200" $
          get "/items" `shouldRespondWith` 200

    context "with range headers" $ do
      context "of acceptable range" $ do
        it "succeeds with partial content" $ do
          r <- request methodGet "/items"
            (rangeHdrs $ ByteRangeFromTo 0 1) ""
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-1/[0-9]+"
            simpleStatus r `shouldBe` status206

        it "understands open-ended ranges" $
          request methodGet "/items"
            (rangeHdrs $ ByteRangeFrom 0) ""
              `shouldRespondWith` 200

        it "returns an empty body when there are no results" $
          request methodGet "/menagerie"
            (rangeHdrs $ ByteRangeFromTo 0 1) ""
              `shouldRespondWith` 204 {
                matchHeaders = []
              }

      it "allow one-item requests" $ do
        r <- request methodGet "/items"
                      (rangeHdrs $ ByteRangeFromTo 0 0) ""
        liftIO $do
          simpleHeaders r `shouldSatisfy`
            matchHeader "Content-Range" "0-0/15"
          simpleStatus r `shouldBe` partialContent206

      it "handles ranges beyond collection length via truncation" $ do
        r <- request methodGet "/items"
                      (rangeHdrs $ ByteRangeFromTo 10 100) ""
        liftIO $ do
          simpleHeaders r `shouldSatisfy`
            matchHeader "Content-Range" "10-14/15"
          simpleStatus r `shouldBe` partialContent206

      context "of invalid range" $
        it "false with 416 for offside range" $
          request methodGet "/items"
            (rangeHdrs $ ByteRangeFromTo 1 0) ""
            `shouldRespondWith` 416
