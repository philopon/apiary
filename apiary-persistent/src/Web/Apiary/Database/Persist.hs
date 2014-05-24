{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

module Web.Apiary.Database.Persist (
    -- * runner
      withWithSqlPool

    -- * execute sql
    , runSql

    -- * types
    , HasPersist

    -- * reexport
    , module Database.Persist.Sql
    ) where

import Database.Persist.Sql
import Control.Monad.Trans.Resource
import Data.Reflection

type HasPersist = Given ApiaryPersistState

newtype ApiaryPersistState = ApiaryPersistState
    { getPool    :: ConnectionPool
    }

runSql :: (MonadBaseControl IO m, HasPersist) => SqlPersistT (ResourceT m) a -> m a
runSql a =
    runResourceT $
    runSqlPool a (getPool given)

withWithSqlPool :: (forall a. (ConnectionPool -> m a) -> m a)
                -> (HasPersist => m b) -> m b
withWithSqlPool with m = with $ \pool -> 
    give (ApiaryPersistState pool) m
