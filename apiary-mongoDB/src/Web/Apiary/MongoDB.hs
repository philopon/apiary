{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Web.Apiary.MongoDB
    ( MongoDB, MongoDBConfig(..), MongoQuery
    -- * initializer
    , initMongoDB, initHerokuMongoDB
    -- * query
    , access
    -- * reexports
    , module Data.Bson
    , module Database.MongoDB.Connection
    , module Database.MongoDB.Query
    , module Database.MongoDB.Admin
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Control.Exception.Lifted
import Control.Monad.Apiary.Action
import Control.Monad.Apiary

import Web.Apiary
import Web.Apiary.Heroku

import qualified Database.MongoDB as MongoDB

import Data.Default.Class
import Data.Time(NominalDiffTime)
import Data.Pool
import Data.Apiary.Compat
import Data.Apiary.Extension
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Data.Bson
import Database.MongoDB.Connection hiding (close, isClosed, connect, connect')
import Database.MongoDB.Query hiding (Query, access)
import Database.MongoDB.Admin

type MongoQuery = MongoDB.Query

data MongoDB = MongoDB (Pool Pipe) MongoDBConfig
instance Extension MongoDB

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

initMongoDB' :: (MonadBaseControl IO m, MonadIO m)
             => MongoDBConfig -> (MongoDB -> m a) -> m a
initMongoDB' conf@MongoDBConfig{..} m =
    bracket (liftIO bra) (liftIO . destroyAllResources) (\a -> m (MongoDB a conf))
  where
    bra = createPool (MongoDB.connect' mongoDBTimeout mongoDBHost)
        MongoDB.close 1 connectionIdleTime numConnection

initMongoDB :: (MonadIO m, MonadBaseControl IO m)
            => MongoDBConfig -> Initializer' m MongoDB
initMongoDB conf = initializerBracket' (initMongoDB' conf)

getMongoDBConfig :: T.Text -> MongoDBConfig -> MongoDBConfig
getMongoDBConfig s0 cfg =
    let (_,      s1)    = T.breakOnEnd "://" s0
        (user,   s2) = T.break (== ':') s1
        (passwd, s3) = T.break (== '@') (T.tail s2)
        (host_,  s4) = first T.unpack $ T.break (== ':') (T.tail s3)
        (port,   s5) = T.break (== '/') (T.tail s4)
        db = if T.null s5 then "" else T.tail s5
    in cfg { mongoDBHost     = either (const $ host host_) (Host host_ . PortNumber . fst) (T.decimal port) 
           , mongoDBAuth     = Just (user, passwd)
           , mongoDBDatabase = db
           }

-- | initialize MongoDB extension using heroku service.
-- 
-- compatible:
--
-- * MongoHQ
-- * MongoLab
-- * MongoSoup
initHerokuMongoDB :: (MonadIO m, MonadBaseControl IO m, Has Heroku exts)
                  => MongoDBConfig -> Initializer m exts (MongoDB ': exts)
initHerokuMongoDB conf = initializerBracket $ \exts m -> do
    let hc = getExtension Proxy exts
    mbConn <- liftIO . runMaybeT $
        MaybeT (getHerokuEnv' "MONGOHQ_URL"   hc) <|>
        MaybeT (getHerokuEnv' "MONGOLAB_URI"  hc) <|>
        MaybeT (getHerokuEnv' "MONGOSOUP_URL" hc)
    let conf' = maybe conf (flip getMongoDBConfig conf) mbConn
    initMongoDB' conf' m

-- | query using 'MongoDBConfig' settings.
--
-- if you want to access other db, other accessmode, please use 'useDb' or 'accessMode'.
class MongoAccess m where
    access :: Action m a -> m a

accessImpl :: (MonadBaseControl IO m, MonadIO m) => Action m a -> MongoDB -> m a
accessImpl m (MongoDB mongo conf) = withResource mongo $ \p ->
    MongoDB.access p (mongoDBAccessMode conf) (mongoDBDatabase conf) $
    maybe (return True) (uncurry auth) (mongoDBAuth conf)
    >>= flip unless (throwIO $ ConnectionFailure $ userError "auth failed.")
    >> m

instance (Has MongoDB exts, MonadBaseControl IO m, MonadIO m) => MongoAccess (ActionT exts prms m) where
    access m = getExt (Proxy :: Proxy MongoDB) >>= accessImpl m

instance (Has MongoDB exts, MonadBaseControl IO m, MonadIO m, Monad actM) => MongoAccess (ApiaryT exts prms actM m) where
    access m = apiaryExt (Proxy :: Proxy MongoDB) >>= accessImpl m

instance (Has MongoDB exts, MonadBaseControl IO m, MonadIO m) => MongoAccess (ReaderT (Extensions exts) m) where
    access m = ask >>= accessImpl m . getExtension (Proxy :: Proxy MongoDB)
