{-# LANGUAGE OverloadedStrings #-}

module PgQuery where

import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.Functor             ((<$>))
import           Data.List                (intercalate, intersperse)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))

import           Database.HDBC            hiding (colNullable, colType)
import           Database.HDBC.PostgreSQL
import qualified Network.HTTP.Types.URI   as Net

import qualified RangeQuery               as R
import Types (SqlRow, getRow)

import qualified Data.Aeson as JSON

data RangedResult = RangedResult
  { rrFrom  :: Int,
    rrTo    :: Int,
    rrTotal :: Int,
    rrBody  :: BL.ByteString
  }

type QuotedSql = (String, [SqlValue])

getRows :: String -> Net.Query -> Maybe R.NonnegRange -> Connection -> IO RangedResult
getRows table qq range conn = do
  query <- populateSql conn
    $ globalAndLimitedCounts schema table <>
      jsonArrayRows
      (selectStarClause schema table
        <> whereClause qq
        <> limitClause range)

  print query

  r <- quickQuery conn query []

  return $ case r of
             [[total, limited_total, json]] ->
               RangedResult 0 (fromSql limited_total) (fromSql total) (fromSql json)
             _ -> RangedResult 0 0 0 ""


  where
    schema = "public"

globalAndLimitedCounts :: String -> String -> QuotedSql
globalAndLimitedCounts schema table =
  (" select (select count(1) from %I.%I), count(t), ", map toSql [schema, table])

whereClause :: Net.Query -> QuotedSql
whereClause qs =
  if null qs then ("", []) else (" where ", []) <> conjunction

  where
    conjunction = mconcat $ intersperse (" and ", []) (map wherePred qs)

wherePred :: Net.QueryItem -> QuotedSql
wherePred (column, predicate) =
  ("%I " <> op <> "%L", map toSql [column, value])

  where
    opCode:rest = BS.split ':' $ fromMaybe "" predicate
    value = BS.intercalate ":" rest
    op = case opCode of
           "eq"  -> "="
           "gt"  -> ">"
           "lt"  -> "<"
           "gte" -> ">="
           "lte" -> "<="
           "neq" -> "<>"
           _     -> "="

limitClause :: Maybe R.NonnegRange -> QuotedSql
limitClause range =
  (" limit %s offset %s", [toSql limit, toSql offset])

  where
    limit = fromMaybe "ALL" $ show <$> (R.limit =<< range)
    offset = fromMaybe 0 $ R.offset <$> range

selectStarClause :: String -> String -> QuotedSql
selectStarClause schema table =
  (" select * from %I.%I t ", map toSql [schema, table])


selectCountClause :: String -> String -> QuotedSql
selectCountClause schema table =
  (" select count(1) from %I.%I t ", map toSql [schema, table])


jsonArrayRows :: QuotedSql -> QuotedSql
jsonArrayRows q =
  ("array_to_json(array_agg(row_to_json(t))) from (", []) <> q <> (") t", [])

insert :: Text -> Text -> SqlRow -> Connection -> IO BL.ByteString
insert schema table row conn = do
  query <- populateSql conn ("insert into %I.%I("++colIds++")" map toSql $ schema:table:cols)
  stmt <- prepare conn (query ++ " values ("++phs++") returning *")
  _ <- execute stmt values
  keys <- getColumnNames stmt
  Just vals <- fetchRow stmt
  let rowMap = fromList $ zip keys vals
  return $ JSON.encode rowMap
  where
    (cols, values) = unzip . getRow $ getRow
    colIds = intercalate ", " $ map (const "%I") cols
    phs = intercalate ", " $ map (const "?") values

populateSql :: Connection -> QuotedSql -> IO String
populateSql conn sql = do
  [[escaped]] <- quickQuery conn q (snd sql)
  return $ fromSql escaped

  where
    q = concat ["select format('", fst sql, "', ", placeholders (snd sql), ")" ]

    placeholders :: [a] -> String
    placeholders = intercalate "," . map (const "?::varchar")
