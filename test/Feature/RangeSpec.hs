{-# LANGUAGE OverloadedStrings #-}

module Feature.RangeSpec where

import           SpecHelper

import           Test.Hspec
import           Test.Hspec.Wai

import           Network.HTTP.Types

import           Network.Wai.Test   (SResponse (simpleHeaders))

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
          r <- request methodGet  "/items"
            (rangerHdrs $ ByteRangeFromTo 0 1) ""
          liftIO $
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-1/[0-9]+"

        it "understands open-ended ranges" $
          request methodGet "/items"
            (rangerHdrs $ ByteRangeFrom 0) ""
              `shouldRespondWith` 200

      context "of invalid range" $
        it "false with 416 for offside range" $
          request methodGet "/items"
            (rangerHdrs $ ByteRangeFromTo 1 0) ""
            `shouldRespondWith` 416
