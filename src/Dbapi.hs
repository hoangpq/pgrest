{-# LANGUAGE OverloadedStrings #-}

module Dbapi where


import           Control.Exception         (try)
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BL

import           Data.Map                  (fromList, intersection, toList)
import           Data.Ranged.Ranges        (emptyRange)
import           Types                     (SqlRow)

import           Database.HDBC.SqlValue    (SqlValue (..))

import           Database.HDBC.PostgreSQL  (Connection)
import           Database.HDBC.Types       (SqlError, seErrorMsg)
import           Debug.Trace
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai

import           PgQuery
import           PgStructure               (primaryKeyColumns, printColumns,
                                            printTables)
import           RangeQuery

import qualified Data.Aeson                as JSON
import           Data.Text                 (pack, unpack)

import           Data.Convertible.Base     (convert)
import           Data.Text.Encoding        (encodeUtf8)
import           Network.HTTP.Base         (urlEncodeVars)

data AppConfig = AppConfig
  { configDbUri :: String,
    configPort  :: Int
  }

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
app :: Connection -> Application
app conn req respond = do
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
        jsonBodyAction req (\row -> do
          allvals <- insert table row conn
          keys <- primaryKeyColumns schema (unpack table) conn

          let keyvals = allvals `intersection`  fromList (zip keys $ repeat SqlNull)
          let params = urlEncodeVars $ map (\t -> (fst t, "eq." <> convert (snd t) :: String)) $ toList keyvals

          return $ responseLBS status201
            [ jsonContentType
            , (hLocation, "/" <> encodeUtf8 table <> "?" <> BS.pack params)
            ] ""
        )

      (_, _) ->
        return $ responseLBS status404 [] ""

  respond $ either sqlErrorHandler id r
  where
    path = pathInfo req
    qq = queryString req
    verb = requestMethod req
    range = requestRange (requestHeaders req)
    schema = "public"

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

sqlErrorHandler :: SqlError -> Response
sqlErrorHandler e =
  responseLBS status400 [] $ BL.fromChunks [BS.pack (seErrorMsg e)]
