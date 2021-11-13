{-# LANGUAGE OverloadedStrings #-}

module Dbapi where


import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BL
import           Data.String.Conversions   (cs)

import           Data.Map                  (Map, fromList, intersection, toList)
import           Data.Ranged.Ranges        (emptyRange)
import           Types                     (SqlRow, getRow)

import           Database.HDBC.PostgreSQL  (Connection)
import           Database.HDBC.Types       (SqlError, seErrorMsg)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai

import           PgQuery
import           PgStructure               (Column (colName), columns,
                                            primaryKeyColumns, printColumns,
                                            printTables)
import           RangeQuery

import qualified Data.Aeson                as JSON

import           Data.Convertible.Base     (convert)
import           Data.Text.Encoding        (encodeUtf8)

import           Control.Monad             (join)
import           Data.List                 (sort)

import           Network.HTTP.Base         (urlEncodeVars)
import           Network.HTTP.Types.URI

import           Control.Arrow             ((***))

import qualified Data.Set                  as Set
import           Network.Wai.Internal

import           Data.Maybe                (isJust)

import           Data.Text                 (Text, strip)

data AppConfig = AppConfig
  { configDbUri   :: String
  , configPort    :: Int
  , configSslCert :: FilePath
  , configSSlKey  :: FilePath
  , configPool    :: Int
  }

jsonContentType :: (HeaderName, BS.ByteString)
jsonContentType = (hContentType, "application/json")

jsonBodyAction :: Request -> (SqlRow -> IO Response) -> IO Response
jsonBodyAction req handler = do
  parse <- jsonBody req
  case parse of
    Left err -> return $ responseLBS status400 [jsonContentType] json
      where json = JSON.encode . JSON.object $ [("error", JSON.String $ "Failed to parse json payload" <> cs err)]
    Right body -> handler body

jsonBody :: Request -> IO (Either String SqlRow)
jsonBody = fmap JSON.eitherDecode . strictRequestBody

filterByKeys :: Ord a => Map a b -> [a] -> Map a b
filterByKeys m keys =
  if null keys then m else
    m `intersection` fromList (zip keys $ repeat undefined)

app :: Connection -> Application
app conn req respond =
  respond =<< case (path, verb) of
    ([], _) ->
      responseLBS status200 [jsonContentType] <$> printTables schema conn

    ([table], "OPTIONS") ->
      responseLBS status200 [jsonContentType] <$>
        printColumns schema (cs table) conn

    (["favicon.ico"], "GET") ->
      return $ responseLBS status200 [] ""

    ([table], "GET") ->
      if range == Just emptyRange
        then return $ responseLBS status416 [] "HTTP Range error"
        else do
          r <- respondWithRangedResult <$> getRows (cs schema) (cs table) qq range conn
          -- print $ show $ lookup hRange $ responseHeaders r
          let canonical = urlEncodeVars $ sort $
                          map (join (***) cs) $
                          parseSimpleQuery $
                          rawQueryString req

          return $ addHeaders [
            ("Content-Location",
              "/" <> cs table <> "?" <> cs canonical
            )] r

    ([table], "POST") ->
      jsonBodyAction req (\row -> do
        allvals <- insert (cs schema) table row conn
        keys <- map cs <$> primaryKeyColumns (cs schema) (cs table) conn

        let params = urlEncodeVars $ map (\t -> (fst t, "eq." <> convert (snd t) :: String)) $ toList $ filterByKeys allvals keys
        return $ responseLBS status201
          [ jsonContentType
          , (hLocation, "/" <> encodeUtf8 table <> "?" <> cs params)
          ] ""
      )

    ([table], "PUT") ->
      jsonBodyAction req (\row -> do
        keys <- map cs <$> primaryKeyColumns (cs schema) (cs table) conn

        let specifiedKeys = map (cs . fst) qq
        if Set.fromList keys /= Set.fromList specifiedKeys
          then return $ responseLBS status405 []
                "You much specify all and only primary key as params"
          else
            if isJust cRange
              then return $ responseLBS status400 []
                    "Content-Range is not allow in PUT request"
            else do
              cols <- columns (cs table) conn
              let colNames = Set.fromList $ map (cs . colName) cols

              let specifiedCols = Set.fromList $ map fst $ getRow row
              if colNames == specifiedCols then do
                allvals <- upsert (cs schema) table row qq conn

                let params = urlEncodeVars $ map (\t -> (fst t, "eq." <> convert (snd t) :: String))
                             $ toList $ filterByKeys allvals keys

                return $ responseLBS status201
                  [jsonContentType
                  , (hLocation, "/" <> encodeUtf8 table <> "?" <> cs params)
                  ] ""

              else return $ if Set.null colNames then responseLBS status404 [] ""
                else responseLBS status400 []
                  "You must specify all column in PUT request"

            )

    (_, _) ->
      return $ responseLBS status404 [] ""

  -- respond $ either sqlErrorHandler id r
  where
    path = pathInfo req
    qq = queryString req
    verb = requestMethod req
    range = requestRange (requestHeaders req)
    cRange = requestContentRange (requestHeaders req)
    schema = "public"

respondWithRangedResult :: RangedResult -> Response
respondWithRangedResult rr =
  responseLBS status [
      jsonContentType,
      ( "Content-Range",
        if total == 0 || from > total
        then "*/" <> cs (show total)
        else cs (show from) <> "-"
          <> cs (show to) <> "/"
          <> cs (show total)
      )
    ] (rrBody rr)

    where
      from = rrFrom rr
      to = rrTo rr
      total = rrTotal rr
      status
        | from > total = status416
        | total == 0 = status204
        | (1 + to - from) < total = status206
        | otherwise = status200

sqlErrorHandler :: SqlError -> Response
sqlErrorHandler e =
  responseLBS status400 [] $ BL.fromChunks [cs (seErrorMsg e)]

addHeaders :: ResponseHeaders -> Response -> Response
addHeaders hdrs (ResponseFile s headers fp m) =
                 ResponseFile s (headers ++ hdrs) fp m
addHeaders hdrs (ResponseBuilder s headers b) =
                ResponseBuilder s (headers ++ hdrs) b
addHeaders hdrs (ResponseStream s headers b) =
                ResponseStream s (headers ++ hdrs) b
addHeaders hdrs (ResponseRaw s resp) =
                ResponseRaw s (addHeaders hdrs resp)
