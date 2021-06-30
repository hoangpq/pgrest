{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Feature.RangeSpec where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           SpecHelper

import           Dbapi               (app)
import           Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (prepareAppDb "schema" $ app cfg) $ do
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

  it "list views in schema" $
    get "/" `shouldRespondWith` [json|
      [{"schema":"public","name":"auto_incrementing_pk","insertable":true}]
    |]

  describe "Table info" $
    it "avaibale with OPTIONS verb" $
        -- {{{ big json object
        options "/auto_incrementing_pk" `shouldRespondWith` [json|
        {
            "pkey":["id"],
            "columns":{
            "inserted_at":{
                "precision":null,
                "updatable":true,
                "schema":"public",
                "name":"inserted_at",
                "type":"timestamp with time zone",
                "maxLen":null,
                "nullable":true,
                "position":4},
            "id":{
                "precision":32,
                "updatable":true,
                "schema":"public",
                "name":"id",
                "type":"integer",
                "maxLen":null,
                "nullable":false,
                "position":1},
            "non_nullable_string":{
                "precision":null,
                "updatable":true,
                "schema":"public",
                "name":"non_nullable_string",
                "type":"character varying",
                "maxLen":null,
                "nullable":false,
                "position":3},
            "nullable_string":{
                "precision":null,
                "updatable":true,
                "schema":"public",
                "name":"nullable_string",
                "type":"character varying",
                "maxLen":null,
                "nullable":true,
                "position":2}}
        }
        |]
        -- }}}
