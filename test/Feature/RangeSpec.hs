{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Feature.RangeSpec where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           SpecHelper

import           Dbapi               (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (prepareAppDb "schema" $ app cfg) $ do
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

  it "responds with 'hello'" $
    get "/" `shouldRespondWith` [json|
      [{"schema":"public","name":"auto_incrementing_pk","insertable":true}]
    |]
