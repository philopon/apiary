{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , I.session
    , I.checkToken
    -- * Reexport
    -- | deleteCookie
    , module Web.Apiary.Cookie
    ) where

import Web.Apiary

import Data.Apiary.Extension
import Data.Apiary.Compat

import Control.Monad.Apiary.Action
import qualified Web.Apiary.ClientSession.Internal as I
import Web.Apiary.Cookie (deleteCookie)
import qualified Data.ByteString as S

initSession :: MonadIO m => I.SessionConfig -> Initializer' m I.Session
initSession c = initializer' $ I.makeSession c

setSession :: (Has I.Session exts, MonadIO m)
           => S.ByteString -> S.ByteString
           -> ActionT exts prms m ()
setSession k v = do
    sess <- getExt (Proxy :: Proxy I.Session)
    I.setSession sess k v

getSessionConfig :: (Has I.Session exts, Monad m)
                 => ActionT exts prms m I.SessionConfig
getSessionConfig = do
    sess <- getExt (Proxy :: Proxy I.Session)
    return $ I.sessionConfig sess

setSessionWith :: (Has I.Session exts, MonadIO m)
               => I.SessionConfig
               -> S.ByteString -> S.ByteString
               -> ActionT exts prms m ()
setSessionWith cfg k v = do
    sess <- getExt (Proxy :: Proxy I.Session)
    I.setSession sess { I.sessionConfig = cfg } k v


-- | create crypto random (generate random by AES CTR(cprng-aes package) and encode by base64),
--
-- set it client session cookie, set XSRF-TOKEN header(when Just angularXsrfCookieName),
--
-- and return value. since 0.9.0.0.
csrfToken :: (Has I.Session exts, MonadIO m) => ActionT exts prms m S.ByteString
csrfToken = getExt (Proxy :: Proxy I.Session) >>= I.csrfToken
