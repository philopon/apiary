{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Web.Apiary.Database.Persist
    ( Persist
    -- * initializer
    , Migrator(..), With
    , initPersist,     initPersistNoLog
    , initPersistPool, initPersistPoolNoLog
    -- ** low level
    , initPersist', initPersistPool'
    -- * query
    , RunSQL(runSql)
    -- * filter
    , sql
    ) where

import qualified Data.Pool as Pool
import Control.Monad(void, mzero)
import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad.Logger(NoLoggingT(runNoLoggingT))
import Control.Monad.Trans.Reader(ReaderT(..), runReaderT, ask)
import Control.Monad.Trans.Control(MonadBaseControl)
import Web.Apiary.Logger(LogWrapper, runLogWrapper)

import qualified Database.Persist.Sql as Sql

import Web.Apiary(Html)
import Control.Monad.Apiary.Action(ActionT, applyDict)
import Control.Monad.Apiary.Filter(focus, Filter, Doc(DocPrecondition))
import qualified Network.Routing.Dict as Dict
import qualified Network.Routing as R
import Data.Proxy.Compat(Proxy(..))
import GHC.TypeLits.Compat(KnownSymbol)
import Data.Apiary.Extension
    (Has, Initializer, initializer, Extensions, Extension, MonadExts, getExt)

data Migrator
    = Logging Sql.Migration
    | Silent  Sql.Migration
    | Unsafe  Sql.Migration
    | NoMigrate

data Persist
    = PersistPool Sql.ConnectionPool
    | PersistConn Sql.SqlBackend

instance Extension Persist

type With c m = forall a. (c -> m a) -> m a

initPersist' :: (MonadIO n, MonadBaseControl IO n, Monad m) 
             => (forall a. Extensions exts -> n a -> m a)
             -> With Sql.SqlBackend n -> Migrator -> Initializer m exts (Persist ': exts)
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
            => With Sql.SqlBackend (LogWrapper exts m) -> Sql.Migration
            -> Initializer m exts (Persist ': exts)
initPersist with = initPersist' runLogWrapper with . Logging

initPersistNoLog :: (MonadIO m, MonadBaseControl IO m) 
                 => With Sql.SqlBackend (NoLoggingT m)
                 -> Sql.Migration -> Initializer m es (Persist ': es)
initPersistNoLog with = initPersist' (const runNoLoggingT) with . Silent

initPersistPool' :: (MonadIO n, MonadBaseControl IO n, Monad m)
                 => (forall a. Extensions exts -> n a -> m a)
                 -> With Sql.ConnectionPool n -> Migrator -> Initializer m exts (Persist ': exts)
initPersistPool' run with migr = initializer $ \es -> run es $
    with $ \pool -> do
        Pool.withResource pool $ doMigration migr
        return (PersistPool pool)

initPersistPool :: (MonadIO m, MonadBaseControl IO m)
                => With Sql.ConnectionPool (LogWrapper exts m) -> Sql.Migration
                -> Initializer m exts (Persist ': exts)
initPersistPool with = initPersistPool' runLogWrapper with . Logging

initPersistPoolNoLog :: (MonadIO m, MonadBaseControl IO m)
                     => With Sql.ConnectionPool (NoLoggingT m)
                     -> Sql.Migration -> Initializer m es (Persist ': es)
initPersistPoolNoLog with = initPersistPool' (const runNoLoggingT) with . Silent

doMigration :: (MonadIO m, MonadBaseControl IO m) => Migrator -> Sql.SqlBackend -> m ()
doMigration migr conn = case migr of
    Logging m -> runReaderT (Sql.runMigration m) conn
    Silent  m -> runReaderT (void $ Sql.runMigrationSilent m) conn
    Unsafe  m -> runReaderT (Sql.runMigrationUnsafe m) conn
    NoMigrate -> return ()

-- | execute sql in action.
class RunSQL m where
    runSql :: Sql.SqlPersistT m a -> m a

runSql' :: MonadBaseControl IO m => Sql.SqlPersistT m a -> Persist -> m a
runSql' a persist = case persist of
    PersistPool p -> Sql.runSqlPool a p
    PersistConn c -> Sql.runSqlConn a c

instance (Has Persist es, MonadExts es m, MonadBaseControl IO m) => RunSQL m where
    runSql a = getExt (Proxy :: Proxy Persist) >>= runSql' a

instance (MonadBaseControl IO m) => RunSQL (ReaderT Persist m) where
    runSql a = ask >>= runSql' a

-- | filter by sql query. since 0.9.0.0.
sql :: (KnownSymbol k, Has Persist exts, MonadBaseControl IO actM, k Dict.</ prms)
    => Maybe Html -- ^ documentation.
    -> proxy k
    -> Sql.SqlPersistT (ActionT exts '[] actM) a
    -> (a -> Maybe b) -- ^ result check function. Nothing: fail filter, Just a: success filter and add parameter.
    -> Filter exts actM m prms (k Dict.:= b ': prms)
sql doc k q p = focus (maybe id DocPrecondition doc) Nothing $ R.raw "sql" $ \d t ->
    fmap p (runSql $ hoistReaderT (applyDict Dict.emptyDict) q) >>= \case
        Nothing -> mzero
        Just a  -> return (Dict.add k a d, t)

hoistReaderT :: (forall b. m b -> n b) -> ReaderT r m a -> ReaderT r n a
hoistReaderT f m = ReaderT $ \b -> f (runReaderT m b)
{-# INLINE hoistReaderT #-}
