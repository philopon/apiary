{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Web.Apiary.Memcached
    ( Memcached, CacheConfig(..), MemcachedConfig(..)
    -- * initializer
    , initMemcached, initHerokuMemcached

    -- * raw query
    , memcached

    -- * cache
    , cache, cacheMaybe
    ) where

import Web.Apiary(MonadIO(..))
import Web.Apiary.Heroku(Heroku, getHerokuEnv')

import Control.Applicative((<$>), (<$), (<*>), (<|>))
import Control.Monad.Trans.Maybe(MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.Control(MonadBaseControl, control)
import Control.Monad.Apiary.Action(ActionT)

import Data.Default.Class(Default(..))
import Data.Apiary.Extension
    (Has, Extension, Initializer', initializerBracket'
    , Initializer, initializerBracket, getExtension, getExt
    )
import Data.Proxy.Compat(Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.ByteString.Lazy as L

import qualified Database.Memcached.Binary.IO as Memcached
import qualified Database.Memcached.Binary.IO as IO
import qualified Database.Memcached.Binary.Maybe as Maybe

data Memcached = Memcached Memcached.Connection MemcachedConfig
instance Extension Memcached

data CacheConfig = CacheConfig
    { cacheFlags        :: Memcached.Key -> Memcached.Flags
    , cacheExpiry       :: Memcached.Expiry
    , cacheNotHitExpiry :: Memcached.Expiry
    }

instance Default CacheConfig where
    def = CacheConfig (\_ -> 0) 0 0

data MemcachedConfig = MemcachedConfig
    { connectInfo :: Memcached.ConnectInfo
    , cacheConfig :: Maybe CacheConfig
    }

instance Default MemcachedConfig where
    def = MemcachedConfig def Nothing

initMemcached :: MonadBaseControl IO m => MemcachedConfig -> Initializer' m Memcached
initMemcached cfg = initializerBracket' $ \m -> control $ \run -> 
    Memcached.withConnection (connectInfo cfg) (\c -> run $ m (Memcached c cfg))

getHerokuConfig :: T.Text -> MemcachedConfig -> Heroku -> MaybeT IO MemcachedConfig
getHerokuConfig pfx ci exts = do
    svr <- MaybeT $ getHerokuEnv' (pfx `T.append` "_SERVERS")  exts
    usr <- liftIO $ getHerokuEnv' (pfx `T.append` "_USERNAME") exts
    pwd <- liftIO $ getHerokuEnv' (pfx `T.append` "_PASSWORD") exts

    let (hst, prtTxt) = T.breakOnEnd ":" svr
    prt <- either fail (return . fst) $ T.decimal prtTxt

    let auth = Memcached.Plain <$> (T.encodeUtf8 <$> usr) <*> (T.encodeUtf8 <$> pwd)

    return ci {connectInfo = (connectInfo ci)
        { Memcached.connectHost = T.unpack $ T.init hst
        , Memcached.connectPort = Memcached.PortNumber prt
        , Memcached.connectAuth =
            maybe id (\a -> (a:)) auth $ Memcached.connectAuth (connectInfo ci)
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
    Memcached.withConnection (connectInfo cfg') (\c -> run $ m (Memcached c cfg'))

memcached :: (Has Memcached exts, MonadIO m)
          => (Memcached.Connection -> IO a) -> ActionT exts prms m a
memcached q = do
    Memcached conn _ <- getExt Proxy
    liftIO $ q conn

cache :: (MonadIO m, Has Memcached exts)
      => Memcached.Key -> ActionT exts prms m Memcached.Value
      -> ActionT exts prms m Memcached.Value
cache ky actn = do
    Memcached conn cfg <- getExt Proxy
    case cacheConfig cfg of
        Nothing -> actn
        Just cc -> liftIO (Maybe.get_ ky conn) >>= \case
            Just cr -> return cr
            Nothing -> do
                ar <- actn
                liftIO $ IO.set (cacheFlags cc ky)
                    (cacheExpiry cc) ky ar conn
                return ar

cacheMaybe :: (MonadIO m, Has Memcached exts)
           => Memcached.Key -> ActionT exts prms m (Maybe Memcached.Value)
           -> ActionT exts prms m (Maybe Memcached.Value)
cacheMaybe ky actn = do
    Memcached conn cfg <- getExt Proxy
    case cacheConfig cfg of
        Nothing -> actn
        Just cc -> liftIO (Maybe.get_ ky conn) >>= \case
            Just cr -> case L.uncons cr of
                Just (0, _) -> return Nothing
                Just (1, s) -> return (Just s)
                _           -> actStore cc conn
            Nothing -> actStore cc conn
  where
    actStore cc conn = actn >>= liftIO . \case
        Nothing -> Nothing <$ IO.set (cacheFlags cc ky) (cacheNotHitExpiry cc) ky "\0" conn
        Just ar -> Just ar <$ IO.set (cacheFlags cc ky) (cacheExpiry cc) ky (1 `L.cons` ar) conn
