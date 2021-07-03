{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Dbapi
import           Network.Wai.Handler.Warp  hiding (Connection)

import           Database.HDBC.PostgreSQL  (connectPostgreSQL')
import           Options.Applicative       hiding (columns)

import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

-- argument parser
argParser :: Parser AppConfig
argParser =
  AppConfig
    <$> strOption
      ( long "db"
          <> short 'd'
          <> metavar "URI"
          <> value "postgres://hoangpq:hoangpq@localhost:5432/odoo-10"
          <> help "database  uri to expose, e.g. postgres://user:pass@host:port/database"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "NUMBER"
          <> value 3333
          <> help "port number on which to run HTTP server"
      )

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)
  let port = configPort conf
  let dburi = configDbUri conf

  Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)

  conn <- connectPostgreSQL' dburi
  run port $ app conn
  where
    describe = progDesc "create a REST API to an existing Postgres database"

test :: IO Int
test = do
  tvar <- newTVarIO 1000
  let add5 = readTVar tvar >>= \val -> writeTVar tvar (val + 5)

  let addActions = replicate 100 add5 :: [STM ()]
  mapConcurrently atomically addActions

  atomically (readTVar tvar)

data Pair a = Pair a a deriving Show
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

getPassphrase :: MaybeT IO String
getPassphrase = liftIO getLine
