{-# LANGUAGE OverloadedStrings #-}

module PgQuerySpec where

import           Test.Hspec

import           Database.HDBC
import           Database.HDBC.PostgreSQL

import           PgQuery                  (insert)
import           Types                    (SqlRow (..))

loadFixture :: String -> IO Connection
loadFixture name = do
  sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
  conn <- connectPostgreSQL "postgres://localhost:5432/dbapi_test"
  runRaw conn "drop schema if exists public cascade"
  runRaw conn "create schema public"
  runRaw conn sql
  commit conn
  return conn

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll (loadFixture "schema") $ do
  describe "insert" $
    it "can insert into an empty table" $ \conn -> do
      _ <- insert "auto_incrementing_pk" (SqlRow [
          ("non_nullable_string", toSql ("a string that isn't null" :: String))
        ]) conn
      r <- quickQuery conn "select count(1) from auto_incrementing_pk" []
      commit conn
      [[toSql (1 :: Int)]] `shouldBe` r

  describe "insert again" $
    it "is true" $ \_ ->
      True `shouldBe` True
