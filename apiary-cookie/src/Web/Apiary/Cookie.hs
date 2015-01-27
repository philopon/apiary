{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Apiary.Cookie 
    ( -- * setter
      setCookie
    , deleteCookie
    -- * getter
    , getCookies
    -- * filter
    , cookie
    , cookie'
    -- * Reexport
    -- | SetCookie(..)
    , module Web.Cookie
    ) where

import Control.Applicative((<$>))

import qualified Network.Wai as Wai

import Web.Cookie (SetCookie(..))
import qualified Web.Cookie as Cookie

import Control.Monad.Apiary.Action(ActionT, getHeaders, addHeader)
import Control.Monad.Apiary.Filter

import Data.Apiary.Compat(KnownSymbol, symbolVal)
import Data.Apiary.Dict(type (</))
import Data.Apiary.Param(Strategy(..))

import Data.Maybe(mapMaybe)
import Data.Time(UTCTime(UTCTime), Day(ModifiedJulianDay))
import Data.Monoid((<>))
import Blaze.ByteString.Builder(toByteString)
import Text.Blaze.Html(toHtml)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
cond p t f a = if p a then t a else f a
{-# INLINE cond #-}

-- | cookie filter. since 0.5.1.0.
--
-- can use like 'query' function.
--
-- example:
--
-- @
-- cookie [key|foo|] (pFirst pInt)  -- get first Int parameter from foo.
-- cookie [key|bar|] (pOption pDouble)  -- get first Double parameter from bar, allows no cookie.
-- cookie [key|baz|] (pMany (pMaybe pString))  -- get zero or more baz cookies. allows cookie decrypt failure.
-- cookie [key|baz|] (Proxy :: Proxy (LimitSome [int|100|] ByteString)) -- get raw cookies up to 100 entries.
-- @
cookie :: (Strategy w, Monad actM, k </ prms, KnownSymbol k)
       => proxy k
       -> w S.ByteString
       -> Filter exts actM m prms (SNext w k S.ByteString prms)
cookie k p = function (DocPrecondition $ toHtml (symbolVal k) <> " cookie required") $ \l r ->
    strategy p k (map (Just . snd) . filter ((SC.pack (symbolVal k) ==) . fst) $ cookie' r) l

cookie' :: Wai.Request -> [(S.ByteString, S.ByteString)]
cookie' = 
    concatMap Cookie.parseCookies .
    mapMaybe (cond (("cookie" ==) . fst) (Just . snd) (const Nothing)) .
    Wai.requestHeaders

getCookies :: Monad m => ActionT exts prms m [(S.ByteString, S.ByteString)]
getCookies =
    concatMap (Cookie.parseCookies . snd) .
    filter (("cookie" ==) . fst) <$>
    getHeaders

-- | delete cookie. since 0.6.1.0.
deleteCookie :: Monad m => S.ByteString -> ActionT exts prms m ()
deleteCookie k = setCookie Cookie.def
    { setCookieName    = k 
    , setCookieExpires = Just $ UTCTime (ModifiedJulianDay 0) 0
    , setCookieMaxAge  = Just 0
    }

-- | set raw cookie header.
setCookie :: Monad m => SetCookie -> ActionT exts prms m ()
setCookie =
    addHeader "set-cookie" . toByteString . Cookie.renderSetCookie
