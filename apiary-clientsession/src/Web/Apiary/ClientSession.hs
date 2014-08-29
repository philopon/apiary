{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web.Apiary.ClientSession
    ( I.Session
    -- * config
    , I.SessionConfig(..), I.KeySource(..)
    , I.embedKeyConfig, I.embedDefaultKeyConfig
    -- * initializer
    , initSession
    -- * getter
    , getSessionConfig
    -- * setter
    , setSession
    , csrfToken
    -- ** with sessionConfig
    , setSessionWith
    -- * filter
    , session
    , checkToken
    -- * Reexport
    -- | deleteCookie
    , module Web.Apiary.Cookie
    ) where

import Web.Apiary

import Data.Apiary.Extension
import Data.Apiary.Proxy

import qualified Web.Apiary.ClientSession.Internal as I
import Web.Apiary.Cookie (deleteCookie)
import qualified Data.ByteString as S
import Control.Monad.Apiary.Filter.Internal.Strategy

initSession :: MonadIO m => I.SessionConfig -> Initializer' m I.Session
initSession c = initializer $ I.withSession c return

setSession :: (Has I.Session exts, MonadIO m)
           => S.ByteString -> S.ByteString
           -> ActionT exts m ()
setSession k v = do
    sess <- getExt (Proxy :: Proxy I.Session)
    I.setSession sess k v

getSessionConfig :: (Has I.Session exts, Monad m)
                 => ActionT exts m I.SessionConfig
getSessionConfig = do
    sess <- getExt (Proxy :: Proxy I.Session)
    return $ I.sessionConfig sess

setSessionWith :: (Has I.Session exts, MonadIO m)
               => I.SessionConfig
               -> S.ByteString -> S.ByteString
               -> ActionT exts m ()
setSessionWith cfg k v = do
    sess <- getExt (Proxy :: Proxy I.Session)
    I.setSession sess { I.sessionConfig = cfg } k v

session :: (Has I.Session exts, Query a, Strategy w, MonadIO actM)
        => S.ByteString -> w a
        -> ApiaryT exts (SNext w prms a) actM m ()
        -> ApiaryT exts prms actM m ()
session k w m = do
    sess <- apiaryExt (Proxy :: Proxy I.Session)
    I.session sess k w m

-- | create crypto random (generate random by AES CTR(cprng-aes package) and encode by base64),
--
-- set it client session cookie, set XSRF-TOKEN header(when Just angularXsrfCookieName),
--
-- and return value. since 0.9.0.0.
csrfToken :: (Has I.Session exts, MonadIO m) => ActionT exts m S.ByteString
csrfToken = getExt (Proxy :: Proxy I.Session) >>= I.csrfToken

-- | check csrf token. since 0.9.0.0.
checkToken :: (Has I.Session exts, MonadIO actM)
           => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
checkToken m = do
    sess <- apiaryExt (Proxy :: Proxy I.Session)
    I.checkToken sess m

