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
    , maxAge'   :: Maybe DiffTime
    , path'     :: Maybe S.ByteString
    , domain'   :: Maybe S.ByteString
    , httpOnly' :: Bool
    , secure'   :: Bool
    }

data SessionConfig = SessionConfig
    { keyFile  :: FilePath
    , maxAge   :: Maybe DiffTime
    , path     :: Maybe S.ByteString
    , domain   :: Maybe S.ByteString
    , httpOnly :: Bool
    , secure   :: Bool
    }

instance Default SessionConfig where
    def = SessionConfig
        defaultKeyFile (Just (24 * 60 * 60)) Nothing Nothing True True

type HasSession = ?webApiaryClientSessionSession :: Session

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
cond p t f a = if p a then t a else f a

withSession :: MonadIO m => SessionConfig -> (HasSession => m b) -> m b
withSession SessionConfig{..} m = do
    k <- liftIO $ getKey keyFile
    let ?webApiaryClientSessionSession = Session
            k maxAge path domain httpOnly secure
    m

setMaxAge :: SetCookie -> Maybe DiffTime -> IO SetCookie
setMaxAge s (Just a) = do
    t <- getCurrentTime
    return s { setCookieExpires = Just $ addUTCTime (realToFrac a) t
             , setCookieMaxAge  = Just a
             }
setMaxAge s Nothing = return s

encryptValue :: HasSession => SetCookie -> IO SetCookie
encryptValue s = do
    v' <- encryptIO (key ?webApiaryClientSessionSession) (setCookieValue s)
    return $ s { setCookieValue = v' }
    
setRawSession :: (MonadIO m, HasSession) => Maybe DiffTime -> SetCookie -> ActionT m ()
setRawSession age s = do
    s' <- liftIO $ encryptValue =<< setMaxAge s age
    setCookie s'

setSessionWith :: (MonadIO m, HasSession)
               => (SetCookie -> SetCookie) -- ^ postprocess
               -> S.ByteString -- ^ key
               -> S.ByteString -- ^ value
               -> ActionT m ()
setSessionWith f k v = do
    let Session{..} = ?webApiaryClientSessionSession
    setRawSession maxAge' $ f def
        { setCookieName     = k 
        , setCookieValue    = v
        , setCookiePath     = path'
        , setCookieDomain   = domain'
        , setCookieHttpOnly = httpOnly'
        , setCookieSecure   = secure'
        }

setSession :: (MonadIO m, HasSession)
           => S.ByteString -- ^ key
           -> S.ByteString -- ^ value
           -> ActionT m ()
setSession = setSessionWith id

session :: (Strategy w, Query a, HasSession, Monad n, Functor n)
        => S.ByteString -> Proxy (w a) -> ApiaryT' (SNext w as a) n m b -> ApiaryT' as n m b
session ky p = function $ \l r -> readStrategy readQuery ((ky ==) . fst) p
    (map (\(k,b) -> (k, decrypt (key ?webApiaryClientSessionSession) b)) $ cookie' r) l
