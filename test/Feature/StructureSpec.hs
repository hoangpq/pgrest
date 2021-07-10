{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Feature.StructureSpec where

import           SpecHelper

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec :: Spec
spec = with appWithFixture' $ do

  describe "GET /" $
    it "lists view in schema" $
      get "/" `shouldRespondWith`
        [json| [
          {"schema":"public","name":"auto_incrementing_pk","insertable":true}
        , {"schema":"public","name":"compound_pk","insertable":true}
        , {"schema":"public","name":"items","insertable":true}
        , {"schema":"public","name":"menagerie","insertable":true}
        , {"schema":"public","name":"no_pk","insertable":true}
        , {"schema":"public","name":"simple_pk","insertable":true}
        ] |]
        {matchStatus = 200}

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
