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

import           Data.Char

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

requireAlphaNum :: String -> Maybe String
requireAlphaNum password =
  case all isAlphaNum password of
    False -> Nothing
    True  -> return password

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x:xs) =
  case isSpace x of
    True  -> cleanWhitespace xs
    False -> return $ x:xs

data StringOrValue a = Str String | Val a deriving Show

instance Monad StringOrValue where
  Val a >>= f = f a
  Str s >>= _ = Str s

instance Applicative StringOrValue where
  pure val -> Val val
  Val val f <*>

newtype Username = Username String deriving Show
newtype Password = Password String deriving Show
newtype Error = Error String deriving Show

data User = User Username Password deriving Show


validateUsername :: String -> Either Error Username
validateUsername username =
  case length username > 15 of
    True -> Left (Error "Username cannot be longer than 15 characters.")
    False -> Right $ Username username

validatePassword :: String -> Either Error Password
validatePassword password =
  case length password > 20 of
    True -> Left $ (Error "Your password cannot be longer than \
                          \ 20 characters.")
    False -> Right $ Password password

makeUser :: String -> String -> Either Error User
makeUser name password = User <$> validateUsername name
                              <*> validatePassword password

makeUserTmpPassword :: String -> Either Error User
makeUserTmpPassword username =
  User <$> validateUsername username
       <*> (Right $ Password "temporaryPassword")

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Val v) f = f v
bindStringOrValue (Str xs) f = Str xs
