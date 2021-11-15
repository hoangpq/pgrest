module Dbapi where


import qualified Data.ByteString.Char8     as BS
-- import qualified Data.ByteString.Lazy      as BL
import           Data.String.Conversions   (cs)

import           Data.Map                  (Map, fromList, intersection)
-- import           Data.Ranged.Ranges        (emptyRange)

import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai



import qualified Data.Aeson                as JSON

import           PgStructure



import qualified Hasql.Connection          as H
import qualified Hasql.Session             as H.Session

-- import qualified Hasql.Statement           as H
-- import qualified Hasql.Decoders            as HD


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
  if null keys then m else
    m `intersection` fromList (zip keys $ repeat undefined)

app :: H.Connection -> Application
app conn req respond =
  respond =<< case (path, verb) of
    -- table schema
    ([], _) -> do
      -- execute query
      result <- H.Session.run (H.Session.statement schema tables) conn
      case result of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          return $ responseLBS status500 [] ""
        Right rows -> do
          let json = JSON.encode rows
          return $ responseLBS status200 [jsonContentType] (cs json)
    -- 404
    (_, _) ->
      return $ responseLBS status404 [] ""

  where
    path = pathInfo req
    -- qq = queryString req
    verb = requestMethod req
    schema = "public"

