{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Web.Apiary.MongoDB
    ( MongoDB, MongoDBConfig(..), MongoQuery
    , initMongoDB, access
    , module Data.Bson
    , module Database.MongoDB.Connection
    , module Database.MongoDB.Query
    , module Database.MongoDB.Admin
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Exception.Lifted

import Web.Apiary

import qualified Database.MongoDB as MongoDB

import Data.Default.Class
import Data.Time(NominalDiffTime)
import Data.Pool
import Data.Apiary.Proxy
import Data.Apiary.Extension

import Data.Bson
import Database.MongoDB.Connection hiding (close, isClosed, connect, connect')
import Database.MongoDB.Query hiding (Query, access)
import Database.MongoDB.Admin

type MongoQuery = MongoDB.Query

data MongoDB = MongoDB (Pool Pipe) MongoDBConfig

data MongoDBConfig = MongoDBConfig
    { mongoDBTimeout     :: Secs
    , mongoDBHost        :: Host
    , mongoDBAuth        :: Maybe (Username, Password)
    , mongoDBDatabase    :: Database
    , mongoDBAccessMode  :: AccessMode
    , numConnection      :: Int
    , connectionIdleTime :: NominalDiffTime
    }

instance Default MongoDBConfig where
    def = MongoDBConfig 6 (host "localhost") Nothing "" master 1 20

initMongoDB :: (MonadIO m, MonadBaseControl IO m)
            => MongoDBConfig -> Initializer' m MongoDB
initMongoDB conf@MongoDBConfig{..} = initializerBracket $ \m ->
    bracket (liftIO bra) (liftIO . destroyAllResources) (\a -> m (MongoDB a conf))
  where
    bra = createPool (MongoDB.connect' mongoDBTimeout mongoDBHost)
        MongoDB.close 1 connectionIdleTime numConnection

access :: (Has MongoDB exts, MonadBaseControl IO m, MonadIO m)
       => Action (ActionT exts m) a -> ActionT exts m a
access m = do
    MongoDB mongo conf <- getExt (Proxy :: Proxy MongoDB)
    withResource mongo $ \p ->
        MongoDB.access p (mongoDBAccessMode conf) (mongoDBDatabase conf) $
        maybe (return True) (uncurry auth) (mongoDBAuth conf)
        >>= flip unless (throwIO $ ConnectionFailure $ userError "auth failed.")
        >>  m
