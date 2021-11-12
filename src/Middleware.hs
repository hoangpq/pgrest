{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Middleware where

import           Data.Pool                (Pool, withResource)
import           Database.HDBC            (runRaw)

import           Database.HDBC.PostgreSQL (Connection)

import           Control.Exception.Base   (SomeException, catch, finally, throw)
import           Network.Wai              (Application)

withDBConnection :: Pool Connection -> (Connection -> Application) -> Application
withDBConnection pool app req respond =
  withResource pool $ \conn ->
    app conn req respond

inTransaction :: (Connection -> Application) -> Connection -> Application
inTransaction app conn req respond =
  finally (runRaw conn "begin" >> app conn req respond) (runRaw conn "commit")

withSavePoint :: (Connection -> Application) -> Connection -> Application
withSavePoint app conn req respond = do
  runRaw conn "savepoint req_sp"
  catch (app conn req respond) (\e -> let _ = e::SomeException in
    runRaw conn "rollback to req_sp" >> throw e)
