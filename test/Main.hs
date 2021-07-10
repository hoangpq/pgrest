module Main where

import           Database.HDBC (disconnect, runRaw)
import           Spec
import           SpecHelper    (loadFixture, openConnection)
import           Test.Hspec

main :: IO ()
main = do
    c <- openConnection
    runRaw c "drop schema if exists public cascade"
    runRaw c "drop schema if exists \"1\" cascade"
    loadFixture "schema" c
    disconnect c
    putStrLn "before spec"
    hspec spec
    putStrLn "after spec"
