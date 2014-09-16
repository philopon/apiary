{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Web.Apiary.Memcached
    ( Memcached, CacheConfig(..), MemcachedConfig(..)
    , initMemcached, memcached
    , cache, cacheMaybe
    ) where

import Web.Apiary
import Data.Default.Class
import Data.Apiary.Extension
import Data.Apiary.Proxy
import qualified Data.Binary as B

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

initMemcached :: MemcachedConfig -> Initializer' IO Memcached
initMemcached cfg = initializerBracket $ \m ->
    withConnection (connectInfo cfg) (\c -> m (Memcached c cfg))

memcached :: (Has Memcached exts, MonadIO m)
          => (Connection -> IO a) -> ActionT exts m a
memcached q = do
    Memcached conn _ <- getExt Proxy
    liftIO $ q conn

cache :: (MonadIO m, Has Memcached exts)
      => Key -> ActionT exts m Value -> ActionT exts m Value
cache key actn = do
    Memcached conn cfg <- getExt Proxy
    case cacheConfig cfg of
        Nothing -> actn
        Just cc -> liftIO (Maybe.get_ key conn) >>= \case
            Just cr -> return cr
            Nothing -> do
                ar <- actn
                liftIO $ set (cacheFlags cc key)
                    (cacheExpiry cc) key ar conn
                return ar

cacheMaybe :: (MonadIO m, Has Memcached exts)
           => Key -> ActionT exts m (Maybe Value)
           -> ActionT exts m (Maybe Value)
cacheMaybe key actn = do
    Memcached conn cfg <- getExt Proxy
    case cacheConfig cfg of
        Nothing -> actn
        Just cc -> liftIO (Maybe.get_ key conn) >>= \case
            Just cr -> return $ B.decode cr
            Nothing -> actn >>= \case
                Nothing -> do
                    liftIO $ set (cacheFlags cc key)
                        (cacheNotHitExpiry cc) key (B.encode (Nothing :: Maybe Value)) conn
                    return Nothing
                Just ar -> do
                    liftIO $ set (cacheFlags cc key)
                        (cacheExpiry cc) key (B.encode $ Just ar) conn
                    return (Just ar)
