{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.ClientSession
    ( HasSession
    , I.SessionConfig(..), I.KeySource(..)
    , withSession
    , I.embedKeyConfig, I.embedDefaultKeyConfig
    -- * setter
    , setSession
    , csrfToken
    -- * filter
    , session
    , checkToken
    -- * Reexport
    , module Data.Default.Class
    -- | deleteCookie
    , module Web.Apiary.Cookie
    ) where

import Web.Apiary

import Data.Default.Class
import qualified Web.Apiary.ClientSession.Internal as I
import Web.Apiary.Cookie (deleteCookie)
import Data.Reflection
import qualified Data.ByteString as S
import Control.Monad.Apiary.Filter.Internal.Strategy

type HasSession = Given I.Session

withSession :: MonadIO m => I.SessionConfig -> (HasSession => m b) -> m b
withSession c m = I.withSession c (\s -> give s m)

setSession :: (MonadIO m, HasSession)
           => S.ByteString -> S.ByteString -> ActionT m ()
setSession = I.setSession given

-- | create crypto random (generate random by AES CTR(cprng-aes package) and encode by base64),
--
-- set it client session cookie, set XSRF-TOKEN header(when Just angularXsrfCookieName),
--
-- and return value. since 0.9.0.0.
csrfToken :: (MonadIO m, HasSession) => ActionT m S.ByteString
csrfToken = I.csrfToken given

session :: (Functor n, MonadIO n, Strategy w, Query a, HasSession)
        => S.ByteString -> proxy (w a)
        -> ApiaryT (SNext w as a) n m b -> ApiaryT as n m b
session = I.session given

-- | check csrf token. since 0.9.0.0.
checkToken :: (Functor n, MonadIO n, HasSession)
           => ApiaryT c n m a -> ApiaryT c n m a
checkToken = I.checkToken given
