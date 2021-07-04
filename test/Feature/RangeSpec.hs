{-# LANGUAGE OverloadedStrings #-}

module Feature.RangeSpec where

import           SpecHelper

import           Test.Hspec
import           Test.Hspec.Wai

spec :: Spec
spec = with appWithFixture' $
  describe "GET /view" $
    context "without range headers" $
      context "with response under server size limit" $
        it "returns whole range with status 206" $
          get "/auto_incrementing_pk" `shouldRespondWith` 206

