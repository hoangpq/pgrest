{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Dbapi
import           Network.Wai.Handler.Warp

import           Database.HDBC.PostgreSQL    (connectPostgreSQL')
import           Options.Applicative         hiding (columns)

import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import           Network.Wai.Middleware.Gzip (def, gzip)

import           Control.Exception           (bracket)

import           Data.Pool                   (createPool, destroyAllResources)
import           Database.HDBC               (disconnect)
import           Middleware                  (withDBConnection)

import           Data.List                   (intercalate)

import           Data.Version                (versionBranch)
import           Paths_pgrest                (version)

import           Data.String.Conversions     (cs)

argParser :: Parser AppConfig
argParser =
  AppConfig
    <$> strOption ( long "db" <> short 'd' <> metavar "URI"
        <> value "postgres://hoangpq:hoangpq@localhost:5432/odoo-10"
        <> help "database uri to expose, e.g. postgres://user:pass@host:port/database" )
    <*> option auto ( long "port" <> short 'p' <> metavar "NUMBER" <> value 3333
        <> help "port number on which to run HTTP server" )
    <*> strOption ( long "sslcert" <> short 'c' <> metavar "PATH" <> value "test/test.crt"
        <> help "path to SSL cert file" )
    <*> strOption ( long "sslkey" <> short 'k' <> metavar "PATH" <> value "test/test.key"
        <> help "path to SSL key file" )
    <*> option auto ( long "db-pool"<> metavar "NUMBER" <> value 10
        <> help "Max connections in database pool" )


main :: IO ()
main = do
  -- Parse command line arguments
  conf <- execParser (info (helper <*> argParser) describe)

  bracket
    -- create pool
    (createPool (connectPostgreSQL' (configDbUri conf))
      disconnect 1 600 (configPool conf))
    -- destroy pool
    destroyAllResources
    (\pool -> do
      let port = configPort conf
      Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
      let tls = tlsSettings (configSslCert conf) (configSSlKey conf)
      let settings = setPort port
                   . setServerName (cs $ "pgrest/" <> prettyVersion)
                   $ defaultSettings
      runTLS tls settings $ gzip def .withDBConnection pool $ app
    )
  where
    describe = progDesc "create a REST API to an existing Postgres database"
    prettyVersion = intercalate "." $ map show $ versionBranch version
