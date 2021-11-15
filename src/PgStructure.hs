{-# LANGUAGE DeriveGeneric, QuasiQuotes #-}

module PgStructure where

import           Data.Text       hiding (concat, foldl, map, zipWith)

import           Data.Aeson      (FromJSON, ToJSON)
import           GHC.Generics    (Generic)
import qualified Hasql.Decoders  as HD
import qualified Hasql.Encoders  as HE
import qualified Hasql.Statement as H

data Table = Table {
  tableSchema     :: Text
, tableName       :: Text
, tableInsertable :: Bool
} deriving (Generic, Show)

instance FromJSON Table
instance ToJSON Table

tables :: H.Statement Text [Table]
tables = let
  sql =
      "select table_schema, table_name,\
      \     is_insertable_into\
      \ from information_schema.tables\
      \ where table_schema = $1\
      \ order by table_name"
  encoder = HE.param (HE.nonNullable HE.text)
  decoder =
    HD.rowList $
      Table <$>
        HD.column (HD.nonNullable HD.text) <*>
        HD.column (HD.nonNullable HD.text) <*>
        HD.column (HD.nonNullable HD.bool)
  in H.Statement sql encoder decoder True

