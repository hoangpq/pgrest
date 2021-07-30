{-# LANGUAGE OverloadedStrings #-}
module SpecHelper where

import           Network.Wai               (Application)
import           Network.Wai.Test          (SResponse)
import           Test.Hspec                hiding (pending, pendingWith)
import           Test.Hspec.Wai            as HW

import           Database.HDBC
import           Database.HDBC.PostgreSQL

import           Control.Exception.Base    (bracket)
import           Dbapi                     (AppConfig (..), app)

import qualified Data.ByteString.Char8     as BS
import           Network.HTTP.Types.Header

import qualified Data.HashMap.Strict       as Map

import           Data.CaseInsensitive
import           Text.Regex.TDFA           ((=~))

cfg :: AppConfig
cfg = AppConfig "postgres://postgres:@localhost:5432/dbapi_test" 9000 "test/test.crt" "test/test.key"

openConnection :: IO Connection
openConnection = connectPostgreSQL' $ configDbUri cfg

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection disconnect

withDatabaseConnection' :: (Connection -> IO Connection) -> IO Connection
withDatabaseConnection' = bracket openConnection (return . disconnect)

loadFixture :: String -> Connection -> IO ()
loadFixture name conn = do
    runRaw conn "create schema if not exists public"
    sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
    runRaw conn sql
    commit conn

dbWithSchema :: ActionWith Connection -> IO ()
dbWithSchema action = withDatabaseConnection $ \c -> do
    runRaw  c "begin;"
    action c
    rollback c

dbConnectionWithRollback :: IO Connection
dbConnectionWithRollback = withDatabaseConnection' $ \c -> do
    runRaw c "begin;"
    return c

appWithFixture :: ActionWith Application -> IO ()
appWithFixture action = withDatabaseConnection $ \c -> do
    runRaw  c "begin;"
    action $ app c
    rollback c

appWithFixture' :: IO Application
appWithFixture' = app <$> dbConnectionWithRollback

rangeHdrs :: ByteRange -> [Header]
rangeHdrs r = [rangeUnit, (hRange, renderByteRange r)]

rangeUnit :: Header
rangeUnit = ("Range-Unit" :: CI BS.ByteString, "items")

getHeader :: CI BS.ByteString -> [Header] -> Maybe BS.ByteString
getHeader name headers =
  Map.lookup name $ Map.fromList headers

matchHeader :: CI BS.ByteString -> String -> [Header] -> Bool
matchHeader name valRegex headers =
  maybe False (=~ valRegex) $ getHeader name headers
