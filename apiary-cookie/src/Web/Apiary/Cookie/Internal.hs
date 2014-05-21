{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

module Web.Apiary.Cookie.Internal where

import Network.Wai
import Web.Apiary
import Web.Cookie
import Data.Maybe
import Data.Proxy
import Data.Time

import Control.Monad.Apiary.Filter.Internal
import Control.Monad.Apiary.Filter.Internal.Strategy

import Blaze.ByteString.Builder
import qualified Data.ByteString as S

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
cond p t f a = if p a then t a else f a

-- | cookie filter. since 0.5.1.0.
--
-- can use like 'query' function.
--
-- example:
--
-- @
-- cookie "foo" (pFirst pInt)  -- get first Int parameter from foo.
-- cookie "bar" (pOption pDouble)  -- get first Double parameter from bar, allows no cookie.
-- cookie "baz" (pMany (pMaybe pString))  -- get zero or more baz cookies. allows cookie decrypt failure.
-- cookie "baz" (Proxy :: Proxy (LimitSome [int|100|] ByteString)) -- get raw cookies up to 100 entries.
-- @
cookie :: (Strategy w, Query a)
       => S.ByteString
       -> Proxy (w a)
       -> Apiary (SNext w as a) b
       -> Apiary as b
cookie k p = function $ \l r -> readStrategy (readQuery . Just) ((k ==) . fst) p (cookie' r) l

cookie' :: Request -> [(S.ByteString, S.ByteString)]
cookie' = 
    concatMap parseCookies .
    mapMaybe (cond (("cookie" ==) . fst) (Just . snd) (const Nothing)) .
    requestHeaders

-- | delete cookie. since 0.6.1.0.
deleteCookie :: S.ByteString -> Action ()
deleteCookie k = setCookie def { setCookieName    = k 
                               , setCookieExpires = Just $ UTCTime (ModifiedJulianDay 0) 0
                               , setCookieMaxAge  = Just 0
                               }

-- | set raw cookie header.
setCookie :: SetCookie -> Action ()
setCookie =
    addHeader "set-cookie" . toByteString . renderSetCookie
