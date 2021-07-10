{-# LANGUAGE OverloadedStrings #-}

module Unit.PgQuerySpec where

import           Test.Hspec

import           Database.HDBC

import           PgQuery       (insert)
import           Types         (SqlRow (..))

import           Data.Map      (toList)
import           Data.Text     (pack)
import           SpecHelper

spec :: Spec
spec = around dbWithSchema $
  describe "insert" $ do
    it "can insert into an empty table" $ \conn -> do
      _ <- insert "auto_incrementing_pk" (SqlRow [
          ("non_nullable_string", toSql ("a string that isn't null" :: String))
        ]) conn
      r <- quickQuery conn "select count(1) from auto_incrementing_pk" []
      commit conn
      [[toSql (1 :: Int)]] `shouldBe` r

    it "throws an exception if the PK is not unique" $ \conn -> do
      r <- insert "auto_incrementing_pk" (SqlRow [
        ("non_nullable_string", toSql ("a string" :: String))]) conn
      let row = SqlRow . map (\(k, v) -> (pack k, v)) . toList $ r
      insert "auto_incrementing_pk" row conn `shouldThrow` \e ->
        seState e == "23505"
