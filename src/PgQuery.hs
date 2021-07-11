{-# LANGUAGE OverloadedStrings #-}

module PgQuery where

import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.List                (intercalate, intersperse)
import           Data.Maybe               (fromMaybe)

import           Database.HDBC            hiding (colNullable, colType)
import           Database.HDBC.PostgreSQL
import qualified Network.HTTP.Types.URI   as Net

import qualified RangeQuery               as R
import           Types                    (SqlRow, getRow, sqlRowColumns,
                                           sqlRowValues)

import qualified Data.Map                 as Map

import           Control.Monad            (join)
import           Data.Text                (Text)
import           Debug.Trace

data RangedResult = RangedResult
  { rrFrom  :: Int,
    rrTo    :: Int,
    rrTotal :: Int,
    rrBody  :: BL.ByteString
  } deriving Show

type QuotedSql = (String, [SqlValue])

getRows :: String -> Net.Query -> Maybe R.NonnegRange -> Connection -> IO RangedResult
getRows table qq range conn = do
  query <- populateSql conn
    $ globalAndLimitedCounts schema table <>
      jsonArrayRows
      (selectStarClause schema table
        <> whereClause qq
        <> limitClause range)

  r <- quickQuery conn query []
  return $ case r of
            [[total, _, SqlNull]] -> RangedResult offset 0 (fromSql total) ""
            [[total, limited_total, json]] ->
              RangedResult offset (offset + fromSql limited_total - 1)
                           (fromSql total) (fromSql json)
            _ -> RangedResult 0 0 0 ""
  where
    schema = "public"
    offset = maybe 0 R.offset range

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
    opCode:rest = BS.split '.' $ fromMaybe "." predicate
    value = BS.intercalate "." rest
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
    limit = maybe "ALL" show (R.limit =<< range)
    offset = maybe 0 R.offset range

selectStarClause :: String -> String -> QuotedSql
selectStarClause schema table =
  (" select * from %I.%I t ", map toSql [schema, table])


selectCountClause :: String -> String -> QuotedSql
selectCountClause schema table =
  (" select count(1) from %I.%I t ", map toSql [schema, table])


jsonArrayRows :: QuotedSql -> QuotedSql
jsonArrayRows q =
  ("array_to_json(array_agg(row_to_json(t))) from (", []) <> q <> (") t", [])

insert :: Text -> SqlRow -> Connection -> IO (Map.Map String SqlValue)
insert table row conn = do
  sql    <- populateSql conn $ insertClause table row
  stmt   <- prepare conn sql
  _      <- execute stmt $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return m

upsert :: Text -> SqlRow -> Net.Query -> Connection -> IO (Map.Map String SqlValue)
upsert table row qq conn = do
  sql    <- populateSql conn $ upsertClause table row qq
  stmt   <- prepare conn (traceShow sql sql)
  _      <- execute stmt $ join $ replicate 2 $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return (traceShow m m)

placeholders :: String -> SqlRow -> String
placeholders symbol = intercalate ", " . map (const symbol) . getRow

insertClause :: Text -> SqlRow -> QuotedSql
insertClause table row =
  ("insert into %I.%I (" ++ placeholders "%I" row ++ ")",
    map toSql $ schema : table: sqlRowColumns row)
  <> (" values (" ++ placeholders "?" row ++ ") returning *", sqlRowValues row)
  where
    schema = "public"

insertClauseViaSelect :: Text -> SqlRow -> QuotedSql
insertClauseViaSelect table row =
  ("insert into %I.%I (" ++ placeholders "%I" row ++ ")",
    map toSql $ schema : table : sqlRowColumns row)
  <> (" select " ++ placeholders "?" row, sqlRowValues row)
  where
    schema = "public"

updateClause :: Text -> SqlRow -> QuotedSql
updateClause table row =
  ("update %I.%I set (" ++ placeholders "%I" row ++ ")",
    map toSql $ schema : table : sqlRowColumns row)
  <> (" = (" ++ placeholders "?" row ++ ")", [])
  where
    schema = "public"

upsertClause :: Text -> SqlRow -> Net.Query -> QuotedSql
upsertClause table row qq =
  ("with upsert as (", []) <> updateClause table row
  <> whereClause qq
  <> (" returning *) ", []) <> insertClauseViaSelect table row
  <> (" where not exists (select * from upsert) returning *", [])

populateSql :: Connection -> QuotedSql -> IO String
populateSql conn sql = do
  [[escaped]] <- quickQuery conn q (snd sql)
  return $ fromSql escaped

  where
    q = concat ["select format('", fst sql, "', ", ph (snd sql), ")" ]
    ph :: [a] -> String
    ph = intercalate ", " . map (const "?::varchar")
