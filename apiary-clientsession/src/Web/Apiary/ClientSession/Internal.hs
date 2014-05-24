{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Reflection

import Control.Monad.Apiary.Filter.Internal
import Control.Monad.Apiary.Filter.Internal.Strategy

import qualified Data.ByteString as S

data Session = Session
    { key       :: Key
    , config    :: SessionConfig
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

type HasSession = Given Session

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
cond p t f a = if p a then t a else f a

withSession :: MonadIO m => SessionConfig -> (HasSession => m b) -> m b
withSession cfg@SessionConfig{..} m = do
    k <- liftIO $ getKey keyFile
    let sess = Session k cfg
    give sess m

setMaxAge :: SetCookie -> Maybe DiffTime -> IO SetCookie
setMaxAge s (Just a) = do
    t <- getCurrentTime
    return s { setCookieExpires = Just $ addUTCTime (realToFrac a) t
             , setCookieMaxAge  = Just a
             }
setMaxAge s Nothing = return s

encryptValue :: HasSession => SetCookie -> IO SetCookie
encryptValue s = do
    v' <- encryptIO (key given) (setCookieValue s)
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
    let Session{..} = given
    setRawSession (maxAge config) $ f def
        { setCookieName     = k 
        , setCookieValue    = v
        , setCookiePath     = path config
        , setCookieDomain   = domain config
        , setCookieHttpOnly = httpOnly config
        , setCookieSecure   = secure config
        }

setSession :: (MonadIO m, HasSession)
           => S.ByteString -- ^ key
           -> S.ByteString -- ^ value
           -> ActionT m ()
setSession = setSessionWith id

session :: (Strategy w, Query a, HasSession, Monad n, Functor n)
        => S.ByteString -> Proxy (w a) -> ApiaryT (SNext w as a) n m b -> ApiaryT as n m b
session ky p = function $ \l r -> readStrategy readQuery ((ky ==) . fst) p
    (map (\(k,b) -> (k, decrypt (key given) b)) $ cookie' r) l
