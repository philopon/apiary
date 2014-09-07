{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.Memcached (Memcached, initMemcached, memcached) where

import Web.Apiary
import Data.Apiary.Extension
import Data.Apiary.Proxy

import Database.Memcached.Binary.IO

newtype Memcached = Memcached Connection

initMemcached :: ConnectInfo -> Initializer' IO Memcached
initMemcached i = initializerBracket $
    \m -> withConnection i (\c -> m (Memcached c))

memcached :: (Has Memcached exts, MonadIO m)
          => (Connection -> IO a) -> ActionT exts m a
memcached q = do
    Memcached conn <- getExt Proxy
    liftIO $ q conn
