{-# LANGUAGE OverloadedStrings #-}
module Feature.QuerySpec where

import           Test.Hspec
import           Test.Hspec.Wai

import           SpecHelper

spec :: Spec
spec = with appWithFixture' $ do
  describe "Filter response" $
    context "column equality" $
      it "matches the predicate" $
        get "/items?id=eq.5"
          `shouldRespondWith` ResponseMatcher {
            matchBody = MatchBody $ \_ _ -> Nothing
          , matchStatus = 206
          , matchHeaders = ["Content-Range" <:> "0-0/15"]
          }

  describe "Canonical location" $
    it "Sets Content-Location with alphabetized params" $
      get "/no_pk?b=eq.1&a=eq.1"
        `shouldRespondWith` ResponseMatcher {
          matchBody = MatchBody $ \_ _ -> Nothing
        , matchStatus = 204
        , matchHeaders = ["Content-Location" <:> "/no_pk?a=eq.1&b=eq.1"]
        }

