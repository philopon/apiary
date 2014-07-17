{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Apiary.Database.Persist (
    -- * runner
      withWithSqlPool

    -- * execute sql
    , runSql

    -- * types
    , HasPersist

    -- * filter
    , sql
    , BoolLike(..)

    -- * reexport
    , module Database.Persist.Sql
    ) where

import Database.Persist.Sql

import Web.Apiary

import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.Apiary.Filter.Internal

import Text.Blaze.Html
import Data.Reflection
import Data.Apiary.SList
import Data.Apiary.Document

type HasPersist = Given Persist

newtype Persist = Persist
    { getPool    :: ConnectionPool
    }

runSql :: (MonadBaseControl IO m, HasPersist) => SqlPersistT (ResourceT m) a -> m a
runSql a = runResourceT $ runSqlPool a (getPool given)

withWithSqlPool :: (forall a. (ConnectionPool -> m a) -> m a)
                -> (HasPersist => m b) -> m b
withWithSqlPool with m = with $ \pool -> 
    give (Persist pool) m

class BoolLike a where
  type UnBool a
  unBool :: a -> Maybe (UnBool a)

instance BoolLike (Maybe a) where
    type UnBool (Maybe a) = a
    unBool = id

instance BoolLike [a] where
    type UnBool [a] = [a]
    unBool [] = Nothing
    unBool a  = Just a

-- | filter by sql query. since 0.9.0.0.
sql :: (BoolLike a, Functor n, Monad n, MonadBaseControl IO (ActionT n), HasPersist)
    => Html 
    -> SqlPersistT (ResourceT (ActionT n)) a
    -> ApiaryT (Snoc as (UnBool a)) n m b
    -> ApiaryT as n m b
sql doc p = focus (DocPrecondition doc) $ \l -> do
    r <- runSql p
    maybe empty (\i -> return $ sSnoc l i) $ unBool r
