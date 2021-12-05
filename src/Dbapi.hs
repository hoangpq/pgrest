{-# LANGUAGE FlexibleContexts #-}
module Dbapi where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BS

import Data.String.Conversions (cs)

import Data.Map                  (Map, fromList, intersection)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai

import qualified Data.Aeson  as JSON
import           PgStructure

import qualified Hasql.Connection as H
-- import qualified Hasql.Session    as H.Session
-- import qualified Hasql.Encoders as HE

import Data.List as L
-- import Data.Text

-- import Data.Text
import           Data.String.Conversions (ConvertibleStrings (convertString))
import qualified Hasql.Session           as H

data AppConfig = AppConfig
  { configDbUri   :: String
  , configPort    :: Int
  , configSslCert :: FilePath
  , configSSlKey  :: FilePath
  , configPool    :: Int
  }

jsonContentType :: (HeaderName, BS.ByteString)
jsonContentType = (hContentType, "application/json")

filterByKeys :: Ord a => Map a b -> [a] -> Map a b
filterByKeys m keys =
  if L.null keys then m else
    m `intersection` fromList (L.zip keys $ repeat undefined)

app :: H.Connection -> Application
app conn req respond =
  respond =<< case (path, verb) of
    -- table schema
    ([], _) -> do
      resp <- runStatement conn schema tables
      return $ case resp of
                 Left e -> responseLBS status500 [] $ (cs . show) e
                 Right rows -> responseLBS status200 [jsonContentType] $ JSON.encode rows

    -- structure
    (["structure"], _) -> do
      rows <- dbStructure conn
      return $ responseLBS status200 [jsonContentType] $ JSON.encode rows

    -- columns
    (["allColumns"], _) -> do
      resp <- runStatement conn () allColumns
      return $ case resp of
                 Left e -> responseLBS status500 [] $ cs . show $ e
                 Right rows -> responseLBS status200 [jsonContentType] $ JSON.encode rows
    -- tables
    ([table], _) -> do
      resp <- runStatement conn (schema, table) columns
      return $ case resp of
                 Left e -> responseLBS status500 [] $ cs . show $ e
                 Right rows -> responseLBS status200 [jsonContentType] $ JSON.encode rows

    -- 404
    (_, _) ->
      return $ responseLBS status404 [] ""

  where
    path = pathInfo req
    -- qq = queryString req
    verb = requestMethod req
    schema = "public"

