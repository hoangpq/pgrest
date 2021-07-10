{-# LANGUAGE OverloadedStrings #-}

module Feature.RangeSpec where

import           SpecHelper

import           Test.Hspec
import           Test.Hspec.Wai

import           Network.HTTP.Types

spec :: Spec
spec = with appWithFixture' $
  describe "GET /view" $ do
    context "without range headers" $
      context "with response under server size limit" $
        it "returns whole range with status 200" $
          get "/items" `shouldRespondWith` 200
    context "with range headers" $
      context "of acceptable range" $
        it "succeeds with partial content" $
          request methodGet "/items"
            (rangerHdrs $ ByteRangeFromTo 0 1) ""
              `shouldRespondWith` 206
