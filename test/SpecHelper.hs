
module SpecHelper where

import           Network.Wai
import           Test.Hspec

import           Database.HDBC
import           Database.HDBC.PostgreSQL

import           Control.Exception.Base   (bracket)
import           Dbapi                    (AppConfig (..), app)

cfg :: AppConfig
cfg = AppConfig "postgres://postgres:@localhost:5432/dbapi_test" 9000

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

appWithFixture' :: IO Application
appWithFixture' = app <$> dbConnectionWithRollback
