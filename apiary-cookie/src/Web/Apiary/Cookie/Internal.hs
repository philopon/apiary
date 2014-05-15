{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Web.Apiary.Cookie.Internal where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Web.Apiary
import Web.ClientSession
import Web.Cookie

import Blaze.ByteString.Builder
import qualified Data.ByteString as S
import Data.Default.Class
import Data.Reflection

newtype Cookie = Cookie
    { key :: Key
    }

newtype CookieConfig = CookieConfig
    { keyFile :: FilePath }

instance Default CookieConfig where
    def = CookieConfig defaultKeyFile

-- | Give cookie encryption key.
withCookie :: CookieConfig -> (Given Cookie => IO b) -> IO b
withCookie conf f = do
    k <- getKey $ keyFile conf
    give (Cookie k) f

setCookie :: (MonadIO m, Given Cookie) => SetCookie -> ActionT m ()
setCookie sc = do
    v' <- liftIO $ encryptIO (key given) (setCookieValue sc) 
    let s = toByteString . renderSetCookie $ sc { setCookieValue = v' }
    addHeader "set-cookie" s

-- | get cookies. first Maybe indicate cookie header exists or not, 
-- second Maybe indicate decryption status.
getCookies :: (Monad m, Given Cookie) => ActionT m (Maybe [(S.ByteString, Maybe S.ByteString)])
getCookies = runMaybeT $ do
    raw <- MaybeT $ getRequestHeader "cookie"
    return $ map (\(k,v) -> (k, decrypt (key given) v)) $ parseCookies raw

-- | like 'getCookies', but when cookie header isn't exists, pass next handler.
getCookies' :: (Monad m, Given Cookie) => ActionT m [(S.ByteString, Maybe S.ByteString)]
getCookies' = getCookies >>= maybe mzero return

-- | get cookie of specific key.
getCookie :: (Monad m, Given Cookie) => S.ByteString -> ActionT m (Maybe S.ByteString)
getCookie k = getCookies >>= return . maybe Nothing (join . lookup k)

getCookie' :: (Monad m, Given Cookie) => S.ByteString -> ActionT m S.ByteString
getCookie' k = getCookie k >>= maybe mzero return
