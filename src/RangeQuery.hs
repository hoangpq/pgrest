{-# LANGUAGE OverloadedStrings #-}

module RangeQuery where

import           Network.HTTP.Types.Header

import           Data.Ranged.Boundaries
import           Data.Ranged.Ranges

import qualified Data.ByteString.Char8     as BS
import           Text.Read                 (readMaybe)
import           Text.Regex.TDFA           ((=~))

import           Data.Maybe                (listToMaybe)

type NonnegRange = Range Int

rangeGeq :: Int -> NonnegRange
rangeGeq n =
  Range (BoundaryBelow n) BoundaryAboveAll

rangeLeq :: Int -> NonnegRange
rangeLeq n =
  Range BoundaryBelowAll (BoundaryAbove n)

parseRange :: String -> Maybe NonnegRange
parseRange range = do
  let rangeRegex = "^([0-9]+)-([0-9]*)$" :: String

  parsedRange <- listToMaybe (range =~ rangeRegex :: [[String]])

  let [_, from, to] = readMaybe <$> parsedRange
  let lower = maybe emptyRange rangeGeq from
  let upper = maybe (rangeGeq 0) rangeLeq to

  return $ rangeIntersection lower upper

requestRange :: RequestHeaders -> Maybe (Range Int)
requestRange hdrs = parseRange . BS.unpack =<< lookup hRange hdrs

limit :: NonnegRange -> Maybe Int
limit range =
  case [rangeLower range, rangeUpper range] of
    [BoundaryBelow from, BoundaryAbove to] -> Just (1 + to - from)
    _                                      -> Nothing

offset :: NonnegRange -> Int
offset range =
  case rangeLower range of
    BoundaryBelow from -> from
    _                  -> 0 -- should never happen

