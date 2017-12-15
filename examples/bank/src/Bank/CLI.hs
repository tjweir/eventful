{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Bank.CLI
  ( bankCLIMain
  ) where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (NoLoggingT (..), runNoLoggingT)
import qualified Data.Text                   as T

import           Data.Text.Encoding          (encodeUtf8)
import qualified Database.Persist.Postgresql as DB
import           System.Environment          (lookupEnv)

import           Eventful.Store.Postgresql

import           Bank.CLI.Options
import           Bank.CLI.RunCommand


data Environment
   = Development
   | Production
   | Test
   deriving (Eq, Read, Show)

bankCLIMain :: IO ()
bankCLIMain = do
  -- let pool = makeStore
  pool <- getPool Test
  Options{..} <- runOptionsParser
  liftIO $ flip DB.runSqlPool pool $ do
    void $ DB.runMigrationSilent migrateSqlEvent
  runCLICommand pool optionsCommand
  return ()

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
  s <- getConnectionString e
  let n = 1
  case e of
    Development -> runNoLoggingT (DB.createPostgresqlPool s n)
    Production  -> runNoLoggingT (DB.createPostgresqlPool s n)
    Test        -> runNoLoggingT (DB.createPostgresqlPool s n)

getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e = do
   m <- lookupEnv "DATABASE_URL"
   let s = case m of
         Nothing -> getDefaultConnectionString e
         Just u  -> getDefaultConnectionString e
   return s

getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString e =
   let n = case e of
         Development -> "hairy_development"
         Production  -> "hairy_production"
         Test        -> "hairy_test"
   in  createConnectionString
         [ ("host", "localhost")
         , ("port", "5432")
         , ("user", "tylerweir")
         , ("dbname", n)
         , ("password", "")
         ]

createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
   let f (k, v) = T.concat [k, "=", v]
   in  encodeUtf8 (T.unwords (map f l))
