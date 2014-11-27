{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Monad.Apiary
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Web.Apiary.Logger

import Database.Persist.Sql

import Web.Apiary
import Control.Monad.Apiary.Action
import Control.Monad.Apiary.Filter
import qualified Data.Apiary.Dict as Dict
import Data.Apiary.Compat
import Data.Apiary.Extension

data Migrator
    = Logging Migration
    | Silent  Migration
    | Unsafe  Migration
    | NoMigrate

data Persist
    = PersistPool ConnectionPool
    | PersistConn SqlBackend

instance Extension Persist

type With c m = forall a. (c -> m a) -> m a

initPersist' :: (MonadIO n, MonadBaseControl IO n, Monad m) 
             => (forall a. Extensions exts -> n a -> m a)
             -> With SqlBackend n -> Migrator -> Initializer m exts (Persist ': exts)
initPersist' run with migr = initializer $ \es -> run es $
    with $ \conn -> do
        doMigration migr conn
        return (PersistConn conn)

-- | construct persist extension initializer with no connection pool.
--
-- example: 
--
-- @
-- initPersist (withSqliteConn "db.sqlite") migrateAll
-- @
initPersist :: (MonadIO m, MonadBaseControl IO m) 
            => With SqlBackend (LogWrapper exts m) -> Migration
            -> Initializer m exts (Persist ': exts)
initPersist with = initPersist' runLogWrapper with . Logging

initPersistNoLog :: (MonadIO m, MonadBaseControl IO m) 
                 => With SqlBackend (NoLoggingT m)
                 -> Migration -> Initializer m es (Persist ': es)
initPersistNoLog with = initPersist' (const runNoLoggingT) with . Silent

initPersistPool' :: (MonadIO n, MonadBaseControl IO n, Monad m)
                 => (forall a. Extensions exts -> n a -> m a)
                 -> With ConnectionPool n -> Migrator -> Initializer m exts (Persist ': exts)
initPersistPool' run with migr = initializer $ \es -> run es $
    with $ \pool -> do
        withResource pool $ doMigration migr
        return (PersistPool pool)

initPersistPool :: (MonadIO m, MonadBaseControl IO m)
                => With ConnectionPool (LogWrapper exts m) -> Migration
                -> Initializer m exts (Persist ': exts)
initPersistPool with = initPersistPool' runLogWrapper with . Logging

initPersistPoolNoLog :: (MonadIO m, MonadBaseControl IO m)
                     => With ConnectionPool (NoLoggingT m)
                     -> Migration -> Initializer m es (Persist ': es)
initPersistPoolNoLog with = initPersistPool' (const runNoLoggingT) with . Silent

doMigration :: (MonadIO m, MonadBaseControl IO m) => Migrator -> SqlBackend -> m ()
doMigration migr conn = case migr of
    Logging m -> runReaderT (runMigration m) conn
    Silent  m -> runReaderT (void (runMigrationSilent m)) conn
    Unsafe  m -> runReaderT (runMigrationUnsafe m) conn
    NoMigrate -> return ()

-- | execute sql in action.
class RunSQL m where
    runSql :: SqlPersistT m a -> m a

runSql' :: MonadBaseControl IO m => SqlPersistT m a -> Persist -> m a
runSql' a persist = case persist of
    PersistPool p -> runSqlPool a p
    PersistConn c -> runSqlConn a c

instance (Has Persist es, MonadExts es m, MonadBaseControl IO m) => RunSQL m where
    runSql a = getExt (Proxy :: Proxy Persist) >>= runSql' a

-- | filter by sql query. since 0.9.0.0.
sql :: (Has Persist exts, MonadBaseControl IO actM, Dict.NotMember k prms)
    => Maybe Html -- ^ documentation.
    -> proxy k
    -> SqlPersistT (ActionT exts prms actM) a
    -> (a -> Maybe b) -- ^ result check function. Nothing: fail filter, Just a: success filter and add parameter.
    -> ApiaryT exts (k := b ': prms) actM m () -> ApiaryT exts prms actM m ()
sql doc k q p = focus (maybe id DocPrecondition doc) $ do
    fmap p (runSql q) >>= \case
        Nothing -> mzero
        Just a  -> Dict.insert k a `fmap` getParams
