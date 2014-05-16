{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

module Web.Apiary.Database.Persist (
    -- * configuration
      ApiaryPersistConfig
    , defaultApiaryPersistConfig

    -- * runner
    , withWithSqlPool, withWithSqlPool'

    -- * execute sql
    , runSql, runSql'

    -- * types
    , HasPersist, LogRunner

    -- * reexport
    , module Database.Persist.Sql
    ) where

import Database.Persist.Sql
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource

type HasPersist l =
    ?webApiaryDatabasePersistState :: ApiaryPersistState l

type LogRunner l = forall a. l (ResourceT IO) a -> ResourceT IO a

newtype ApiaryPersistConfig l = ApiaryPersistConfig
    { runLogger  :: LogRunner l
    }

defaultApiaryPersistConfig :: ApiaryPersistConfig NoLoggingT
defaultApiaryPersistConfig = ApiaryPersistConfig runNoLoggingT

data ApiaryPersistState l = ApiaryPersistState
    { runLogger' :: LogRunner l
    , getPool    :: ConnectionPool
    }

runSql :: (MonadBaseControl IO (l (ResourceT IO)), MonadIO m, HasPersist l) => SqlPersistT (l (ResourceT IO)) a -> m a
runSql = runSql' (runLogger' ?webApiaryDatabasePersistState)

runSql' :: (MonadBaseControl IO logger, MonadIO m, HasPersist logger') => (logger a -> ResourceT IO b) -> SqlPersistT logger a -> m b
runSql' run a =
    liftIO .
    runResourceT .
    run $
    runSqlPool a (getPool ?webApiaryDatabasePersistState)

withWithSqlPool :: (forall a. (ConnectionPool -> m a) -> m a)
                -> (HasPersist NoLoggingT => m b) -> m b
withWithSqlPool = withWithSqlPool' defaultApiaryPersistConfig

withWithSqlPool' :: ApiaryPersistConfig logger
                 -> (forall a. (ConnectionPool -> m a) -> m a)
                 -> (HasPersist logger => m b) -> m b
withWithSqlPool' conf with m = with $ \pool -> do
    let ?webApiaryDatabasePersistState = ApiaryPersistState (runLogger conf) pool
    m

