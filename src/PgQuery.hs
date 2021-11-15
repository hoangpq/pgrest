module PgQuery (
QualifiedTable(..)
, RangedResult(..)
) where

import qualified Data.ByteString.Char8 as BS


import qualified Hasql.Decoders        as HD
import qualified Hasql.Encoders        as HE
import qualified Hasql.Session         as H


data RangedResult = RangedResult
  { rrFrom  :: Int,
    rrTo    :: Int,
    rrTotal :: Int,
    rrBody  :: BS.ByteString
  } deriving Show

data QualifiedTable = QualifiedTable {
  qtSchema :: BS.ByteString
, qtName   :: BS.ByteString
} deriving (Show)

