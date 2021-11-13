{-# LANGUAGE OverloadedStrings #-}

module PgQuery (
  getRows
, insert
, upsert
, RangedResult(..)
) where

import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as BL

import           Data.Maybe               (fromMaybe)

import           Database.HDBC            hiding (colNullable, colType)
import           Database.HDBC.PostgreSQL
import qualified Network.HTTP.Types.URI   as Net

import qualified RangeQuery               as R
import           Types                    (SqlRow (..), getRow, sqlRowColumns,
                                           sqlRowValues)

import qualified Data.Map                 as Map

import           Control.Monad            (join)

import           Data.Text                (Text, intercalate)
import           Debug.Trace

import           Data.String.Conversions  (cs)

import qualified Data.List                as L

data RangedResult = RangedResult
  { rrFrom  :: Int,
    rrTo    :: Int,
    rrTotal :: Int,
    rrBody  :: BL.ByteString
  } deriving Show

type Schema = Text

getRows :: Schema -> Text -> Net.Query -> Maybe R.NonnegRange -> Connection -> IO RangedResult
getRows schema table qq range conn = do
  r <- quickQuery conn (cs query) []

  print query

  return $ case r of
            [[total, _, SqlNull]] -> RangedResult offset 0 (fromSql total) ""
            [[total, limited_total, json]] ->
              RangedResult offset (offset + fromSql limited_total - 1)
                           (fromSql total) (fromSql json)
            _ -> RangedResult 0 0 0 ""
  where
    offset = maybe 0 R.offset range
    query = globalAndLimitedCounts schema table <> jsonArrayRows (
        selectStarClause schema table
        <> whereClause qq
        <> limitClause range)

globalAndLimitedCounts :: Text -> Text -> Text
globalAndLimitedCounts schema table =
  "select (select count(1) from " <> schema <> "." <> table <> "), count(t), "

whereClause :: Net.Query -> Text
whereClause qs =
  if null qs then "" else " where " <> conjunction

  where
    cols = [ col | col <- qs, fst col /= "order" ]
    conjunction = mconcat $ L.intersperse " and " (map wherePred cols)

wherePred :: Net.QueryItem -> Text
wherePred (column, predicate) =
  cs column <> "" <> op <> cs value
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

limitClause :: Maybe R.NonnegRange -> Text
limitClause range =
  cs $ " limit " <> limit <> " offset " <> show offset <> ""

  where
    limit = maybe "ALL" show (R.limit =<< range)
    offset = maybe 0 R.offset range

selectStarClause :: Schema -> Text -> Text
selectStarClause schema table =
  " select * from " <> schema <> "." <>  table <> " "

jsonArrayRows :: Text -> Text
jsonArrayRows q =
  "array_to_json(array_agg(row_to_json(t))) from (" <> q <> ") t"


insert :: Schema -> Text -> SqlRow -> Connection -> IO (Map.Map String SqlValue)
insert schema table row conn = do
  stmt   <- prepare conn $ cs sql
  _      <- execute stmt $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return m

  where sql = insertClause schema table row

upsert :: Schema -> Text -> SqlRow -> Net.Query -> Connection -> IO (Map.Map String SqlValue)
upsert schema table row qq conn = do
  stmt   <- prepare conn $ cs sql
  _      <- execute stmt $ join $ replicate 2 $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return (traceShow m m)

  where sql = upsertClause schema table row qq

placeholders :: Text -> SqlRow -> Text
placeholders symbol = intercalate ", " . map (const symbol) . getRow

insertClause :: Schema -> Text -> SqlRow -> Text
insertClause schema table (SqlRow []) =
  "insert into " <> schema <> "." <> table <> " default values returning *"
insertClause schema table row =
  "insert into " <> schema <> "." <> table <> " (" <>
    intercalate ", " (sqlRowColumns row)
  <> ") values (" <> placeholders "?" row <> ") returning *"

insertClauseViaSelect :: Schema -> Text -> SqlRow -> Text
insertClauseViaSelect schema table row =
  "insert into " <> schema <> "." <> table <> "(" <>
    intercalate ", " (sqlRowColumns row)
  <> ") select " <> placeholders "?" row

updateClause :: Schema -> Text -> SqlRow -> Text
updateClause schema table row =
  "update " <> schema <> "." <> table <> " set(" <>
    intercalate "," (sqlRowColumns row)
  <> ") = (" <> placeholders "?" row <> ")"

upsertClause :: Schema -> Text -> SqlRow -> Net.Query -> Text
upsertClause schema table row qq =
  "with upsert as (" <> updateClause schema table row
  <> whereClause qq
  <> " returning *)" <> insertClauseViaSelect schema table row
  <> " where not exists (select * from upsert) returning *"
