{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Feature.InsertSpec where

import           SpecHelper

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Data.Aeson          ((.:))
import qualified Data.Aeson          as JSON

import           Control.Monad       (mzero)

data IncPK = IncPK {
  incId          :: Int
, incNullableStr :: Maybe String
, incStr         :: String
, incInsert      :: String
}

instance JSON.FromJSON IncPK where
  parseJSON (JSON.Object r) = IncPK <$>
    r .: "id" <*>
    r .: "nullable_string" <*>
    r .: "non_nullable_string" <*>
    r .: "inserted_at"
  parseJSON _ = mzero

spec :: Spec
spec = with appWithFixture' $
    describe "Posting new record" $
        context "with no pk supplied" $
            context "into a table with auto-incrementing pk" $
                it "succeeds with 201 and link" $
                    post "/auto_incrementing_pk" [json|
                        {"non_nullable_string":"not_null"} |]
                        `shouldRespondWith` 201 {
                            matchHeaders = ["Location" <:> "/auto_incrementing_pk?id=eq.2"]
                        }

