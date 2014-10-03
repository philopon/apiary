{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Apiary.Memcached
    ( Memcached, CacheConfig(..), MemcachedConfig(..)
    -- * initializer
    , initMemcached, initHerokuMemcached

    -- * raw query
    , memcached

    -- * cache
    , cache, cacheMaybe
    ) where

import Web.Apiary
import Web.Apiary.Heroku

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control
import Control.Monad.Apiary.Action

import Data.Default.Class
import Data.Apiary.Extension
import Data.Apiary.Compat
import qualified Data.Binary as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import Database.Memcached.Binary.IO
import qualified Database.Memcached.Binary.Maybe as Maybe

data Memcached = Memcached Connection MemcachedConfig

data CacheConfig = CacheConfig
    { cacheFlags        :: Key -> Flags
    , cacheExpiry       :: Expiry
    , cacheNotHitExpiry :: Expiry
    }

instance Default CacheConfig where
    def = CacheConfig (\_ -> 0) 0 0

data MemcachedConfig = MemcachedConfig
    { connectInfo :: ConnectInfo
    , cacheConfig :: Maybe CacheConfig
    }

instance Default MemcachedConfig where
    def = MemcachedConfig def Nothing

initMemcached :: MonadBaseControl IO m => MemcachedConfig -> Initializer' m Memcached
initMemcached cfg = initializerBracket' $ \m -> control $ \run -> 
    withConnection (connectInfo cfg) (\c -> run $ m (Memcached c cfg))

getHerokuConfig :: T.Text -> MemcachedConfig -> Heroku -> MaybeT IO MemcachedConfig
getHerokuConfig pfx ci exts = do
    svr <- MaybeT $ getHerokuEnv' (pfx `T.append` "_SERVERS")  exts
    usr <- liftIO $ getHerokuEnv' (pfx `T.append` "_USERNAME") exts
    pwd <- liftIO $ getHerokuEnv' (pfx `T.append` "_PASSWORD") exts

    let (hst, prtTxt) = T.breakOnEnd ":" svr
    prt <- either fail (return . fst) $ T.decimal prtTxt

    let auth = Plain <$> (T.encodeUtf8 <$> usr) <*> (T.encodeUtf8 <$> pwd)

    return ci {connectInfo = (connectInfo ci)
        { connectHost = T.unpack $ T.init hst
        , connectPort = PortNumber prt
        , connectAuth =
            maybe id (\a -> (a:)) auth $ connectAuth (connectInfo ci)
        }}

-- | initialize memcached extension using heroku service.
--
-- compatile:
--
-- * Memcachier
-- * Memcache cloud
--
initHerokuMemcached :: (Has Heroku exts, MonadBaseControl IO m)
                    => MemcachedConfig -> Initializer m exts (Memcached ': exts)
initHerokuMemcached cfg = initializerBracket $ \exts m -> control $ \run -> do
    let hc = getExtension Proxy exts
    cfg'  <- fmap (maybe cfg id) . runMaybeT $
        getHerokuConfig "MEMCACHIER"     cfg hc <|>
        getHerokuConfig "MEMCACHEDCLOUD" cfg hc
    withConnection (connectInfo cfg') (\c -> run $ m (Memcached c cfg'))

memcached :: (Has Memcached exts, MonadIO m)
          => (Connection -> IO a) -> ActionT exts prms m a
memcached q = do
    Memcached conn _ <- getExt Proxy
    liftIO $ q conn

cache :: (MonadIO m, Has Memcached exts)
      => Key -> ActionT exts prms m Value -> ActionT exts prms m Value
cache ky actn = do
    Memcached conn cfg <- getExt Proxy
    case cacheConfig cfg of
        Nothing -> actn
        Just cc -> liftIO (Maybe.get_ ky conn) >>= \case
            Just cr -> return cr
            Nothing -> do
                ar <- actn
                liftIO $ set (cacheFlags cc ky)
                    (cacheExpiry cc) ky ar conn
                return ar

cacheMaybe :: (MonadIO m, Has Memcached exts)
           => Key -> ActionT exts prms m (Maybe Value)
           -> ActionT exts prms m (Maybe Value)
cacheMaybe ky actn = do
    Memcached conn cfg <- getExt Proxy
    case cacheConfig cfg of
        Nothing -> actn
        Just cc -> liftIO (Maybe.get_ ky conn) >>= \case
            Just cr -> return $ B.decode cr
            Nothing -> actn >>= \case
                Nothing -> do
                    liftIO $ set (cacheFlags cc ky)
                        (cacheNotHitExpiry cc) ky (B.encode (Nothing :: Maybe Value)) conn
                    return Nothing
                Just ar -> do
                    liftIO $ set (cacheFlags cc ky)
                        (cacheExpiry cc) ky (B.encode $ Just ar) conn
                    return (Just ar)
