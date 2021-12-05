{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module PgStructure where

import Data.Text hiding (concat, foldl, map, zipWith)

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

import qualified Data.ByteString as BS

import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

import qualified Hasql.Statement as H

import Contravariant.Extras.Contrazip (contrazip2)
import Text.InterpolatedString.Perl6  (q)

import Data.Int (Int32)

import qualified Hasql.Connection as H
import qualified Hasql.Session    as H
import qualified Hasql.Session    as H.Session

import Data.Maybe (mapMaybe)


data Table = Table {
  tableSchema     :: Text
, tableName       :: Text
, tableInsertable :: Bool
} deriving (Generic, Show)

instance FromJSON Table
instance ToJSON Table

data Column = Column {
  colSchema   :: Text
, colTable    :: Text
, colName     :: Text
, colPosition :: Int32
} deriving (Generic, Show)

instance FromJSON Column
instance ToJSON Column

runStatement :: H.Connection -> a -> H.Statement a b
             -> IO (Either H.Session.QueryError  b)
runStatement conn params stmt =
  H.Session.run (H.Session.statement params stmt) conn

dbStructure :: H.Connection -> IO [Table]
dbStructure conn = do
  Right tableList <- runStatement conn () sampleTables
  return tableList

tables :: H.Statement Text [Table]
tables = let
  sql = [q| select table_schema, table_name,
               is_insertable_into
              from information_schema.tables
              where table_schema = $1
              order by table_name |]
  encoder = HE.param (HE.nonNullable HE.text)
  in
    H.Statement sql encoder tableDecoder True

tableDecoder :: HD.Result [Table]
tableDecoder =
    HD.rowList $
      Table <$>
        HD.column (HD.nonNullable HD.text) <*>
        HD.column (HD.nonNullable HD.text) <*>
        HD.column (HD.nonNullable HD.bool)

columns ::H.Statement (Text, Text) [Column]
columns =  let
  sql =
    [q| select info.table_schema as schema, info.table_name as table_name,
           info.column_name as name, info.ordinal_position as position,
           info.is_nullable as nullable, info.data_type as col_type,
           info.is_updatable as updatable,
           info.character_maximum_length as max_len,
           info.numeric_precision as precision,
           info.column_default as default_value,
           array_to_string(enum_info.vals, ',') as enum
       from (
         select table_schema, table_name, column_name, ordinal_position,
                is_nullable, data_type, is_updatable,
                character_maximum_length, numeric_precision,
                column_default, udt_name
           from information_schema.columns
          where table_schema = $1 and table_name = $2
       ) as info
       left outer join (
         select n.nspname as s,
                t.typname as n,
                array_agg(e.enumlabel ORDER BY e.enumsortorder) as vals
         from pg_type t
            join pg_enum e on t.oid = e.enumtypid
            join pg_catalog.pg_namespace n ON n.oid = t.typnamespace
         group by s, n
       ) as enum_info
       on (info.udt_name = enum_info.n)
    order by position |]
  encoder =
    contrazip2
      (HE.param (HE.nonNullable HE.text))
      (HE.param (HE.nonNullable HE.text))
  in
    H.Statement sql encoder columnDecoder True

columnDecoder :: HD.Result [Column]
columnDecoder = HD.rowList $
  Column <$>
    HD.column (HD.nonNullable HD.text) <*>
    HD.column (HD.nonNullable HD.text) <*>
    HD.column (HD.nonNullable HD.text) <*>
    HD.column (HD.nonNullable HD.int4)

allColumns :: H.Statement () [Column]
allColumns = H.Statement sql HE.noParams columnDecoder True
  where
    sql = [q|
        SELECT DISTINCT
        info.table_schema AS schema,
        info.table_name AS table_name,
        info.column_name AS name,
        info.ordinal_position AS position,
        info.is_nullable::boolean AS nullable,
        info.data_type AS col_type,
        info.is_updatable::boolean AS updatable,
        info.character_maximum_length AS max_len,
        info.numeric_precision AS precision,
        info.column_default AS default_value,
        array_to_string(enum_info.vals, ',') AS enum
    FROM (
        -- CTE based on information_schema.columns to remove the owner filter
        WITH columns AS (
            SELECT current_database()::information_schema.sql_identifier AS table_catalog,
                nc.nspname::information_schema.sql_identifier AS table_schema,
                c.relname::information_schema.sql_identifier AS table_name,
                a.attname::information_schema.sql_identifier AS column_name,
                a.attnum::information_schema.cardinal_number AS ordinal_position,
                pg_get_expr(ad.adbin, ad.adrelid)::information_schema.character_data AS column_default,
                    CASE
                        WHEN a.attnotnull OR t.typtype = 'd'::"char" AND t.typnotnull THEN 'NO'::text
                        ELSE 'YES'::text
                    END::information_schema.yes_or_no AS is_nullable,
                    CASE
                        WHEN t.typtype = 'd'::"char" THEN
                        CASE
                            WHEN bt.typelem <> 0::oid AND bt.typlen = (-1) THEN 'ARRAY'::text
                            WHEN nbt.nspname = 'pg_catalog'::name THEN format_type(t.typbasetype, NULL::integer)
                            ELSE 'USER-DEFINED'::text
                        END
                        ELSE
                        CASE
                            WHEN t.typelem <> 0::oid AND t.typlen = (-1) THEN 'ARRAY'::text
                            WHEN nt.nspname = 'pg_catalog'::name THEN format_type(a.atttypid, NULL::integer)
                            ELSE 'USER-DEFINED'::text
                        END
                    END::information_schema.character_data AS data_type,
                information_schema._pg_char_max_length(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS character_maximum_length,
                information_schema._pg_char_octet_length(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS character_octet_length,
                information_schema._pg_numeric_precision(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS numeric_precision,
                information_schema._pg_numeric_precision_radix(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS numeric_precision_radix,
                information_schema._pg_numeric_scale(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS numeric_scale,
                information_schema._pg_datetime_precision(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS datetime_precision,
                information_schema._pg_interval_type(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.character_data AS interval_type,
                NULL::integer::information_schema.cardinal_number AS interval_precision,
                NULL::character varying::information_schema.sql_identifier AS character_set_catalog,
                NULL::character varying::information_schema.sql_identifier AS character_set_schema,
                NULL::character varying::information_schema.sql_identifier AS character_set_name,
                    CASE
                        WHEN nco.nspname IS NOT NULL THEN current_database()
                        ELSE NULL::name
                    END::information_schema.sql_identifier AS collation_catalog,
                nco.nspname::information_schema.sql_identifier AS collation_schema,
                co.collname::information_schema.sql_identifier AS collation_name,
                    CASE
                        WHEN t.typtype = 'd'::"char" THEN current_database()
                        ELSE NULL::name
                    END::information_schema.sql_identifier AS domain_catalog,
                    CASE
                        WHEN t.typtype = 'd'::"char" THEN nt.nspname
                        ELSE NULL::name
                    END::information_schema.sql_identifier AS domain_schema,
                    CASE
                        WHEN t.typtype = 'd'::"char" THEN t.typname
                        ELSE NULL::name
                    END::information_schema.sql_identifier AS domain_name,
                current_database()::information_schema.sql_identifier AS udt_catalog,
                COALESCE(nbt.nspname, nt.nspname)::information_schema.sql_identifier AS udt_schema,
                COALESCE(bt.typname, t.typname)::information_schema.sql_identifier AS udt_name,
                NULL::character varying::information_schema.sql_identifier AS scope_catalog,
                NULL::character varying::information_schema.sql_identifier AS scope_schema,
                NULL::character varying::information_schema.sql_identifier AS scope_name,
                NULL::integer::information_schema.cardinal_number AS maximum_cardinality,
                a.attnum::information_schema.sql_identifier AS dtd_identifier,
                'NO'::character varying::information_schema.yes_or_no AS is_self_referencing,
                'NO'::character varying::information_schema.yes_or_no AS is_identity,
                NULL::character varying::information_schema.character_data AS identity_generation,
                NULL::character varying::information_schema.character_data AS identity_start,
                NULL::character varying::information_schema.character_data AS identity_increment,
                NULL::character varying::information_schema.character_data AS identity_maximum,
                NULL::character varying::information_schema.character_data AS identity_minimum,
                NULL::character varying::information_schema.yes_or_no AS identity_cycle,
                'NEVER'::character varying::information_schema.character_data AS is_generated,
                NULL::character varying::information_schema.character_data AS generation_expression,
                CASE
                    WHEN c.relkind = 'r'::"char" OR (c.relkind = ANY (ARRAY['v'::"char", 'f'::"char"])) AND pg_column_is_updatable(c.oid::regclass, a.attnum, false) THEN 'YES'::text
                    ELSE 'NO'::text
                END::information_schema.yes_or_no AS is_updatable
            FROM pg_attribute a
               LEFT JOIN pg_attrdef ad ON a.attrelid = ad.adrelid AND a.attnum = ad.adnum
               JOIN (pg_class c
               JOIN pg_namespace nc ON c.relnamespace = nc.oid) ON a.attrelid = c.oid
               JOIN (pg_type t
               JOIN pg_namespace nt ON t.typnamespace = nt.oid) ON a.atttypid = t.oid
               LEFT JOIN (pg_type bt
               JOIN pg_namespace nbt ON bt.typnamespace = nbt.oid) ON t.typtype = 'd'::"char" AND t.typbasetype = bt.oid
               LEFT JOIN (pg_collation co
               JOIN pg_namespace nco ON co.collnamespace = nco.oid) ON a.attcollation = co.oid AND (nco.nspname <> 'pg_catalog'::name OR co.collname <> 'default'::name)
            WHERE NOT pg_is_other_temp_schema(nc.oid) AND a.attnum > 0 AND NOT a.attisdropped AND (c.relkind = ANY (ARRAY['r'::"char", 'v'::"char", 'f'::"char"]))
        )
        SELECT
            table_schema,
            table_name,
            column_name,
            ordinal_position,
            is_nullable,
            data_type,
            is_updatable,
            character_maximum_length,
            numeric_precision,
            column_default,
            udt_name
        FROM columns
        WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
    ) AS info
    LEFT OUTER JOIN (
        SELECT
            n.nspname AS s,
            t.typname AS n,
            array_agg(e.enumlabel ORDER BY e.enumsortorder) AS vals
        FROM pg_type t
        JOIN pg_enum e ON t.oid = e.enumtypid
        JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
        GROUP BY s,n
    ) AS enum_info ON (info.udt_name = enum_info.n)
    ORDER BY schema, position
      |]

sampleTables :: H.Statement () [Table]
sampleTables = H.Statement sql HE.noParams tableDecoder True
  where
    sql = [q| select table_schema, table_name,
               is_insertable_into
              from information_schema.tables
              where table_schema = 'public' and
              table_name in ('res_users', 'res_partner')
              order by table_name |]

columnFromRow :: [Table] -> (Text, Text, Text, Int32) -> Maybe Column
columnFromRow _tabs (s, t, n, pos) = Just $ Column s t n pos

decodeColumns :: [Table] -> HD.Result [Column]
decodeColumns tbls =
  mapMaybe (columnFromRow tbls) <$> HD.rowList colRow
  where
    colRow =
      (,,,) <$>
        HD.column (HD.nonNullable HD.text) <*>
        HD.column (HD.nonNullable HD.text) <*>
        HD.column (HD.nonNullable HD.text) <*>
        HD.column (HD.nonNullable HD.int4)

