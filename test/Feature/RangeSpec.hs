{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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

  describe "Table info" $
    it "is available with OPTIONS verb" $
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

  describe "GET /view" $
    context "without range headers" $
      context "with response under server size limit" $
        it "returns whole range with status 206" $
          get "/auto_incrementing_pk" `shouldRespondWith` 206
