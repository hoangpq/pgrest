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
          {"schema":"public","name":"auth","insertable":true}
        , {"schema":"public","name":"auto_incrementing_pk","insertable":true}
        , {"schema":"public","name":"compound_pk","insertable":true}
        , {"schema":"public","name":"items","insertable":true}
        , {"schema":"public","name":"menagerie","insertable":true}
        , {"schema":"public","name":"no_pk","insertable":true}
        , {"schema":"public","name":"simple_pk","insertable":true}
        ] |]
        {matchStatus = 200}

  describe "Table info" $
    it "is available with OPTIONS verb" $
      options "/menagerie" `shouldRespondWith`
        [json| {
          "pkey":["integer"],
          "columns":[
            {"default":null,"precision":32,"updatable":true,"schema":"public","name":"integer","type":"integer","maxLen":null,"enum":null,"nullable":false,"position":1}
          , {"default":null,"precision":53,"updatable":true,"schema":"public","name":"double","type":"double precision","maxLen":null,"enum":null,"nullable":false,"position":2}
          , {"default":null,"precision":null,"updatable":true,"schema":"public","name":"varchar","type":"character varying","maxLen":null,"enum":null,"nullable":false,"position":3}
          , {"default":null,"precision":null,"updatable":true,"schema":"public","name":"boolean","type":"boolean","maxLen":null,"enum":null,"nullable":false,"position":4}
          , {"default":null,"precision":null,"updatable":true,"schema":"public","name":"date","type":"date","maxLen":null,"enum":null,"nullable":false,"position":5}
          , {"default":null,"precision":null,"updatable":true,"schema":"public","name":"money","type":"money","maxLen":null,"enum":null,"nullable":false,"position":6}
          , {"default":null,"precision":null,"updatable":true,"schema":"public","name":"enum","type":"USER-DEFINED","maxLen":null,"enum":["foo","bar"],"nullable":false,"position":7}
          ]
        }
        |]
        {matchStatus = 200}
