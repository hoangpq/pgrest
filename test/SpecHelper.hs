
module SpecHelper where

import           Network.Wai

import           Database.HDBC
import           Database.HDBC.PostgreSQL

import           Dbapi                    (AppConfig (..))

cfg :: AppConfig
cfg = AppConfig "postgres://postgres:@localhost:5432/dbapi_test" 9000

loadFixture :: String -> IO Connection
loadFixture name = do
  sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
  conn <- connectPostgreSQL "postgres://postgres:@localhost:5432/dbapi_test"
  runRaw conn "drop schema if exists public cascade"
  runRaw conn "create schema public"
  runRaw conn sql
  commit conn
  return conn

prepareAppDb :: String -> Application -> IO Application
prepareAppDb = (. return) . (>>) . loadFixture
