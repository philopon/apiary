{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Apiary.ClientSession.Internal where

import Control.Monad.Trans

import Web.Apiary hiding (Default(..))
import Web.Apiary.Cookie
import Web.Apiary.Cookie.Internal
import Web.ClientSession 
import Data.Proxy
import Data.Time
import Data.Default.Class

import Control.Monad.Apiary.Filter.Internal
import Control.Monad.Apiary.Filter.Internal.Strategy

import qualified Data.ByteString as S

data Session = Session
    { key       :: Key
    , maxAge'   :: DiffTime
    , path'     :: Maybe S.ByteString
    , domain'   :: Maybe S.ByteString
    , httpOnly' :: Bool
    , secure'   :: Bool
    }

data SessionConfig = SessionConfig
    { keyFile  :: FilePath
    , maxAge   :: DiffTime
    , path     :: Maybe S.ByteString
    , domain   :: Maybe S.ByteString
    , httpOnly :: Bool
    , secure   :: Bool
    }

instance Default SessionConfig where
    def = SessionConfig
        defaultKeyFile (24 * 60 * 60) Nothing Nothing False False

type HasSession = ?webApiaryClientSessionSession :: Session

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
cond p t f a = if p a then t a else f a

withSession :: SessionConfig -> (HasSession => IO b) -> IO b
withSession SessionConfig{..} m = do
    k <- getKey keyFile
    let ?webApiaryClientSessionSession = Session
            k maxAge path domain httpOnly secure
    m

setSession :: (MonadIO m, HasSession) 
           => S.ByteString -- key
           -> S.ByteString -- value
           -> ActionT m ()
setSession k v = do
    let Session{..} = ?webApiaryClientSessionSession
    v' <- liftIO $ encryptIO key v
    t  <- liftIO getCurrentTime
    setCookie def { setCookieName     = k
                  , setCookieValue    = v'
                  , setCookiePath     = path'
                  , setCookieExpires  = Just $ addUTCTime (realToFrac maxAge') t
                  , setCookieMaxAge   = Just maxAge'
                  , setCookieDomain   = domain'
                  , setCookieHttpOnly = httpOnly'
                  , setCookieSecure   = secure'
                  }

session :: (Strategy w, Query a, HasSession, Monad m)
        => S.ByteString -> Proxy (w a) -> ApiaryT (SNext w as a) m b -> ApiaryT as m b
session ky p = function $ \l r -> readStrategy readQuery ((ky ==) . fst) p
    (map (\(k,b) -> (k, decrypt (key ?webApiaryClientSessionSession) b)) $ cookie' r) l
