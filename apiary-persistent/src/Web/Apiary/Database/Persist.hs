{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.Database.Persist
    ( Persist
    -- * initializer
    , Migrator(..), With
    , initPersist,     initPersistNoLog
    , initPersistPool, initPersistPoolNoLog
    -- ** low level
    , initPersist', initPersistPool'
    -- * query
    , runSql
    -- * filter
    , sql
    ) where

import Data.Pool
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control

import Database.Persist.Sql

import Web.Apiary
import Web.Apiary.Logger
import Data.Apiary.SList
import Data.Apiary.Proxy
import Data.Apiary.Document
import Data.Apiary.Extension
import Data.Apiary.Extension.Internal
import Control.Monad.Apiary.Filter.Internal

data Migrator
    = Logging Migration
    | Silent  Migration
    | Unsafe  Migration
    | NoMigrate

data Persist
    = PersistPool ConnectionPool
    | PersistConn Connection

type With c m = forall a. (c -> m a) -> m a

initPersist' :: (MonadIO n, MonadBaseControl IO n)
             => (forall a. Extensions exts -> n a -> m a)
             -> With Connection n
             -> Migrator
             -> Initializer m exts (Persist ': exts)
initPersist' run with migr = Initializer $ \e ->
    run e $ with $ \conn -> do
        doMigration migr conn
        return $ addExtension (PersistConn conn) e

-- | construct persist extension initializer with no connection pool.
--
-- example: 
--
-- @
-- initPersist (withSqliteConn "db.sqlite") migrateAll
-- @
initPersist :: (MonadIO m, MonadBaseControl IO m, Has Logger exts)
            => With Connection (LogWrapper exts m) -> Migration
            -> Initializer m exts (Persist ': exts)
initPersist w = initPersist' runLogWrapper w . Logging

initPersistNoLog :: (MonadIO m, MonadBaseControl IO m)
                 => With Connection (NoLoggingT m)
                 -> Migration -> Initializer m es (Persist ': es)
initPersistNoLog w = initPersist' (const runNoLoggingT) w . Silent

initPersistPool' :: (MonadIO n, MonadBaseControl IO n)
                 => (forall a. Extensions exts -> n a -> m a)
                 -> With ConnectionPool n
                 -> Migrator
                 -> Initializer m exts (Persist ': exts)
initPersistPool' run with migr = Initializer $ \e ->
    run e $ with $ \pool -> do
        withResource pool $ doMigration migr
        return $ addExtension (PersistPool pool) e

initPersistPool :: (MonadIO m, MonadBaseControl IO m, Has Logger exts)
                => With ConnectionPool (LogWrapper exts m) -> Migration
                -> Initializer m exts (Persist ': exts)
initPersistPool w = initPersistPool' runLogWrapper w . Logging

initPersistPoolNoLog :: (MonadIO m, MonadBaseControl IO m)
                     => With ConnectionPool (NoLoggingT m) -> Migration
                     -> Initializer m exts (Persist ': exts)
initPersistPoolNoLog w = initPersistPool' (const runNoLoggingT) w . Silent

doMigration :: (MonadIO m, MonadBaseControl IO m) => Migrator -> Connection -> m ()
doMigration migr conn = case migr of
    Logging m -> runReaderT (runMigration m) conn
    Silent  m -> runReaderT (void (runMigrationSilent m)) conn
    Unsafe  m -> runReaderT (runMigrationUnsafe m) conn
    NoMigrate -> return ()

-- | execute sql in action.
runSql :: (Has Persist exts, MonadBaseControl IO m)
       => SqlPersistT (ActionT exts m) a -> ActionT exts m a
runSql a = getExt (Proxy :: Proxy Persist) >>= \case
    PersistPool p -> runSqlPool a p
    PersistConn c -> runSqlConn a c

-- | filter by sql query. since 0.9.0.0.
sql :: (Has Persist exts, MonadBaseControl IO actM)
    => Maybe Html -- ^ documentation.
    -> SqlPersistT (ActionT exts actM) a
    -> (a -> Maybe b) -- ^ result check function. Nothing: fail filter, Just a: success filter and add parameter.
    -> ApiaryT exts (b ': prms) actM m () -> ApiaryT exts prms actM m ()
sql doc q p = focus (maybe id DocPrecondition doc) $ \l ->
    fmap p (runSql q) >>= \case
        Nothing -> mzero
        Just a  -> return (a ::: l)
