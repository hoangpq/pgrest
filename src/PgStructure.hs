{-# LANGUAGE OverloadedStrings #-}

module PgStructure where

import           Data.Aeson               ((.=))
import qualified Data.Aeson               as JSON
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (mapMaybe)
import           Database.HDBC            (fromSql, quickQuery, toSql)
import           Database.HDBC.PostgreSQL

import           Debug.Trace

data Table = Table
  { tableSchema     :: String,
    tableName       :: String,
    tableInsertable :: Bool
  }
  deriving (Show)

-- instance to create json
instance JSON.ToJSON Table where
  toJSON v =
    JSON.object
      [ "schema" .= tableSchema v,
        "name" .= tableName v,
        "insertable" .= tableInsertable v
      ]

-- convert database bool to haskell bool
toBool :: String -> Bool
toBool = (== "YES")

data Column = Column
  { colSchema    :: String,
    colTable     :: String,
    colName      :: String,
    colPosition  :: Int,
    colNullable  :: Bool,
    colType      :: String,
    colUpdatable :: Bool,
    colMaxLen    :: Maybe Int,
    colPrecision :: Maybe Int,
    colDefault   :: Maybe String
  }
  deriving (Show)

instance JSON.ToJSON Column where
  toJSON c =
    JSON.object
      [ "schema" .= colSchema c,
        "name" .= colName c,
        "position" .= colPosition c,
        "nullable" .= colNullable c,
        "type" .= colType c,
        "updatable" .= colUpdatable c,
        "maxLen" .= colMaxLen c,
        "precision" .= colPrecision c,
        "default" .= colDefault c
      ]

tables :: String -> Connection -> IO [Table]
tables s conn = do
  r <-
    quickQuery
      conn
      "select table_schema, table_name,\
      \      is_insertable_into\
      \ from information_schema.tables\
      \ where table_schema = ?\
      \ order by table_name"
      [toSql s]
  return $ mapMaybe mkTable r
  where
    mkTable [schema, name, insertable] =
      Just $
        Table
          (fromSql schema)
          (fromSql name)
          (toBool (fromSql insertable))
    mkTable _ = Nothing

columns :: String -> Connection -> IO [Column]
columns t conn = do
  r <-
    quickQuery
      conn
      "select table_schema, table_name, column_name, ordinal_position,\
      \       is_nullable, data_type, is_updatable,\
      \       character_maximum_length, numeric_precision,\
      \       column_default\
      \ from information_schema.columns\
      \ where table_name = ?\
      \ order by column_name"
      [toSql t]

  return $ mapMaybe mkColumn r
  where
    mkColumn [schema, table, name, pos, nullable, colT, updatable, maxlen, precision, defVal] =
      Just $
        Column
          (fromSql schema)
          (fromSql table)
          (fromSql name)
          (fromSql pos)
          (toBool (fromSql nullable))
          (fromSql colT)
          (toBool (fromSql updatable))
          (fromSql maxlen)
          (fromSql precision)
          (fromSql defVal)
    mkColumn _ = Nothing

data TableOptions = TableOptions
  { tblOptcolumns :: [Column],
    tblOptpkey    :: [String]
  }

instance JSON.ToJSON TableOptions where
  toJSON t =
    JSON.object [
      "columns" .= tblOptcolumns t
    , "pkey"    .= tblOptpkey t ]

printTables :: String -> Connection -> IO BL.ByteString
printTables schema conn = JSON.encode <$> tables schema conn

printColumns :: String -> String -> Connection -> IO BL.ByteString
printColumns schema table conn =
  JSON.encode <$> (TableOptions <$> cols <*> pkey)
  where
    cols :: IO [Column]
    cols = columns table conn
    pkey :: IO [String]
    pkey = primaryKeyColumns schema table conn

primaryKeyColumns :: String -> String -> Connection -> IO [String]
primaryKeyColumns s t conn = do
  r <-
    quickQuery
      conn
      (traceShow query query)
      [toSql s, toSql t]

  return $ map fromSql (concat r)
  where
    query =
      "select kc.column_name \
      \ from \
      \  information_schema.table_constraints tc, \
      \  information_schema.key_column_usage kc \
      \ where \
      \  tc.constraint_type = 'PRIMARY KEY' \
      \  and kc.table_name = tc.table_name \
      \  and kc.table_schema = tc.table_schema \
      \  and kc.constraint_name = tc.constraint_name \
      \  and kc.table_schema = ? \
      \  and kc.table_name = ? \
      \ order by kc.column_name"
