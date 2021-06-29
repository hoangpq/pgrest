{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception         (try)
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BL

import           Data.Ranged.Ranges        (emptyRange)
import           Types                     (SqlRow)

import           Data.Text                 (unpack)
import           Database.HDBC.PostgreSQL  (connectPostgreSQL)
import           Database.HDBC.Types       (SqlError, seErrorMsg)
import           Debug.Trace
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative       hiding (columns)
import           PgQuery
import           PgStructure               (printColumns, printTables)
import           RangeQuery

import qualified Data.Aeson                as JSON
import           Data.Text                 (pack, unpack)

data AppConfig = AppConfig
  { configDbUri :: String,
    configPort  :: Int
  }

-- argument parser
argParser :: Parser AppConfig
argParser =
  AppConfig
    <$> strOption
      ( long "db"
          <> short 'd'
          <> metavar "URI"
          <> value "postgres://hoangpq:hoangpq@localhost:5432/odoo-10"
          <> help "database  uri to expose, e.g. postgres://user:pass@host:port/database"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "NUMBER"
          <> value 3333
          <> help "port number on which to run HTTP server"
      )

traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x

jsonContentType :: (HeaderName, BS.ByteString)
jsonContentType = (hContentType, "application/json")

jsonBodyAction :: Request -> (SqlRow -> IO Response) -> IO Response
jsonBodyAction req handler = do
  parse <- jsonBody req
  case parse of
    Left err -> return $ responseLBS status400 [jsonContentType] json
      where json = JSON.encode . JSON.object $ [("error", JSON.String $ pack err)]
    Right body -> handler body

jsonBody :: Request -> IO (Either String SqlRow)
jsonBody = fmap JSON.eitherDecode . strictRequestBody

-- Simplify app with a do block
app :: AppConfig -> Application
app config req respond = do
  conn <- connectPostgreSQL $ configDbUri config
  r <- try $
    case (path, verb) of
      ([], _) ->
        responseLBS status200 [jsonContentType] <$> printTables conn

      ([table], "OPTIONS") ->
        responseLBS status200 [jsonContentType] <$>
          printColumns (unpack table) conn

      (["favicon.ico"], "GET") ->
        return $ responseLBS status200 [] ""

      ([table], "GET") ->
        if range == Just emptyRange
          then return $ responseLBS status416 [] "HTTP Range error"
          else
            respondWithRangedResult <$> getRows (unpack table) qq range conn
      
      ([table], "POST") ->
        jsonBodyAction req (\row ->
          responseLBS status200 [jsonContentType] <$> 
            insert (pack $ show ver) table row conn)
      
      (_, _) ->
        return $ responseLBS status404 [] ""

  respond $ either sqlErrorHandler id r
  where
    ver = "public"
    path = pathInfo req
    qq = queryString req
    verb = requestMethod req
    range = requestRange (requestHeaders req)

respondWithRangedResult :: RangedResult -> Response
respondWithRangedResult rr =
  responseLBS status206 [
      jsonContentType,
      ( "Content-Range",
        if rrTotal rr == 0
        then "*/0"
        else (BS.pack . show . rrFrom ) rr <> "-"
          <> (BS.pack . show . rrTo ) rr <> "/"
          <> (BS.pack . show . rrTotal ) rr
      )
    ] (rrBody rr)
  where
    json = (hContentType, "")

sqlErrorHandler :: SqlError -> Response
sqlErrorHandler e =
  responseLBS status400 [] $ BL.fromChunks [BS.pack (seErrorMsg e)]

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)

  Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
  run (configPort conf) $ app conf
  where
    describe = progDesc "create a REST API to an existing Postgres database"
