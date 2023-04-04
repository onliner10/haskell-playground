{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Eff
  ( someFunc,
  )
where

import Control.Monad.Error.Class as ME
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, runM, type (~>))
import Control.Monad.Freer.Error as FE
import Control.Monad.Freer.Reader as FR
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class as R

fetchPageDummy :: (MonadError String m) => m String
fetchPageDummy = pure "True"

newtype DummyRecord = DummyRecord {getRecord :: Bool} deriving (Show)

newtype ConnectionString = ConnectionString {getConnectionString :: String} deriving (Show)

parsePageDummy :: (MonadError String m) => String -> m DummyRecord
parsePageDummy "True" = pure $ DummyRecord True
parsePageDummy "False" = pure $ DummyRecord False
parsePageDummy x = ME.throwError $ "Unkown string: " ++ x

writeToDbDummy :: (MonadReader ConnectionString m, MonadIO m) => DummyRecord -> m ()
writeToDbDummy r = do
  (ConnectionString connString) <- R.ask
  liftIO $ putStrLn ("writing " ++ show r ++ " to " ++ connString)

mtlParse :: (MonadIO m) => ConnectionString -> ExceptT String m ()
mtlParse connString = do
  pageContent <- fetchPageDummy
  parsed <- parsePageDummy pageContent

  runReaderT (writeToDbDummy parsed) connString

dummyConnString :: ConnectionString
dummyConnString = ConnectionString $ "PSQL://EXAMPLE.COM"

runMtlParse :: IO ()
runMtlParse =
  runExceptT (mtlParse dummyConnString) >>= either printError reportSuccess
  where
    printError e = putStrLn $ "ERROR: " ++ e
    reportSuccess () = putStrLn "SUCCESS"

------ NOW FREER PART

data Scraper r where
  FetchPage :: Scraper String

runScraper :: (Member (FE.Error String) effs, LastMember IO effs) => Eff (Scraper ': effs) ~> Eff effs
runScraper = interpret $ \case
  FetchPage -> pure "True"

data Db r where
  WriteToDb :: DummyRecord -> Db ()

runDb ::
  ( Member (FE.Error String) effs,
    Member (FR.Reader ConnectionString) effs,
    LastMember IO effs
  ) =>
  Eff (Db ': effs) ~> Eff effs
runDb = interpret $ \case
  WriteToDb r -> do
    (connString :: ConnectionString) <- FR.ask
    liftIO $ putStrLn $ "Writing" ++ show r ++ " to databse using connection " ++ show connString
    pure ()

makeEffect ''Scraper
makeEffect ''Db

effParse :: (Member Scraper r, Member (FE.Error String) r, Member Db r) => Eff r ()
effParse = do
  page <- fetchPage
  record <- either FE.throwError pure $ parsePageDummy page

  writeToDb record

runEffParse :: IO ()
runEffParse = do
  result <- runM $ FR.runReader dummyConnString $ FE.runError $ runScraper $ runDb effParse

  either printError reportSuccess result
  where
    printError e = putStrLn $ "ERROR: " ++ e
    reportSuccess () = putStrLn "SUCCESS"

someFunc :: IO ()
someFunc = do
  putStrLn "MTL parse"
  runMtlParse

  putStrLn "Eff parse"
  runEffParse
  putStrLn "Exiting"
