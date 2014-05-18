{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

module Web.Apiary.Cookie.Internal where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Network.Wai
import Web.Apiary
import Web.ClientSession
import Web.Cookie
import Data.Maybe
import Data.Proxy

import Control.Monad.Apiary.Filter.Internal
import Control.Monad.Apiary.Filter.Internal.Query

import Blaze.ByteString.Builder
import qualified Data.ByteString as S

newtype Cookie = Cookie
    { key :: Key
    }

newtype CookieConfig = CookieConfig
    { keyFile :: FilePath }

instance Default CookieConfig where
    def = CookieConfig defaultKeyFile

type HasCookie = ?webApiaryCookieCookie :: Cookie

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
cond p t f a = if p a then t a else f a

-- | cookie filter. since 0.5.1.0.
--
-- can use like 'query' function.
--
-- example:
--
-- @
-- cookie "foo" pFirst pInt  -- get first Int parameter from foo.
-- cookie "bar" pOption pDouble  -- get first Double parameter from bar, allows no cookie.
-- cookie "baz" pMany (pMaybe pString)  -- get zero or more baz cookies. allows cookie decrypt failure.
-- @
cookie :: (Strategy w, Query a, HasCookie, Monad m)
       => S.ByteString
       -> Proxy (w a)
       -> ApiaryT (SNext w as a) m b
       -> ApiaryT as m b
cookie k p = function $ \l r -> readStrategy k p (cookie' r) l

cookie' :: HasCookie => Request -> [(S.ByteString, Maybe S.ByteString)]
cookie' = 
    map (\(k,b) -> (k, decrypt (key ?webApiaryCookieCookie) b)) .
    concatMap parseCookies .
    take 100 . -- avoid hashdos
    mapMaybe (cond (("cookie" ==) . fst) (Just . snd) (const Nothing)) .
    requestHeaders

-- | Give cookie encryption key.
withCookie :: CookieConfig -> (HasCookie => IO b) -> IO b
withCookie conf f = do
    k <- getKey $ keyFile conf
    let ?webApiaryCookieCookie = Cookie k
    f
setCookie :: (MonadIO m, HasCookie) => SetCookie -> ActionT m ()
setCookie sc = do
    v' <- liftIO $ encryptIO (key ?webApiaryCookieCookie) (setCookieValue sc) 
    let s = toByteString . renderSetCookie $ sc { setCookieValue = v' }
    addHeader "set-cookie" s

{-# DEPRECATED getCookies, getCookies', getCookie, getCookie' "use cookie filter" #-}
-- | get cookies. first Maybe indicate cookie header exists or not, 
-- second Maybe indicate decryption status.
getCookies :: (Monad m, HasCookie) => ActionT m (Maybe [(S.ByteString, Maybe S.ByteString)])
getCookies = runMaybeT $ do
    raw <- MaybeT $ getRequestHeader "cookie"
    return $ map (\(k,v) -> (k, decrypt (key ?webApiaryCookieCookie) v)) $ parseCookies raw

-- | like 'getCookies', but when cookie header isn't exists, pass next handler.
getCookies' :: (Monad m, HasCookie) => ActionT m [(S.ByteString, Maybe S.ByteString)]
getCookies' = getCookies >>= maybe mzero return

-- | get cookie of specific key.
getCookie :: (Monad m, HasCookie) => S.ByteString -> ActionT m (Maybe S.ByteString)
getCookie k = getCookies >>= return . maybe Nothing (join . lookup k)

getCookie' :: (Monad m, HasCookie) => S.ByteString -> ActionT m S.ByteString
getCookie' k = getCookie k >>= maybe mzero return
