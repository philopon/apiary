
-- | Cookie support for Apiary.
--
-- @
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-} -- for implicit signature
-- {-# LANGUAGE FlexibleContexts #-} -- for explicit signature
--
-- import Web.Apiary
-- import Web.Apiary.Cookie
-- import Network.Wai.Handler.Warp
-- import qualified Data.ByteString.Lazy.Char8 as L
-- import qualified Data.ByteString.Char8      as S
-- @
-- 
-- 'withCookie' function give 'Cookie' data type for encrypt cookie.
--
-- 'setCookie' function set cookie. cookie value is automatically encrypted.
--
-- @
-- main :: IO ()
-- main = 'withCookie' def $ run 3000 . runApiary def $ do
-- 
--     [capture|/:String|] . action $ \\s -> do
--         'setCookie' (def { setCookieName = "param", setCookieValue = S.pack s })
--         'setCookie' (def { setCookieName = "dog", setCookieValue = "bowwow" })
--         contentType "text/plain"
--         lbs "lucky cookie."
-- 
--     root $ action_ splittedAction
-- @
--
-- 'getCookie' functions, get and auto decrypt cookie.
--
-- @
-- splittedAction :: (Monad m, HasCookie) => ActionT m ()
-- splittedAction = do
--     s <- 'getCookie'' "param"
--     p <- 'getCookie'' "dog"
--     contentType "text/plain"
--     lbs $ L.unlines [L.fromStrict s, L.fromStrict p]
-- @
--
-- * first, access localhost:3000, 404 page not found shown.
--
-- 'getCookie'' function pass next handler when cookie key is not found,
-- and next handler is not exists. so 404.
--
-- * next, you access localhost:3000/hoge, set param=hoge, dog=bowwow to cookie.
--
-- * then access localhost:3000, show hoge\<CR\>bowwow.
--
--

module Web.Apiary.Cookie 
    ( HasCookie
    , CookieConfig(..)
    , withCookie
    -- * setter
    , setCookie
    -- * filter
    , cookie
    -- * getter
    , getCookies, getCookies'
    , getCookie, getCookie'
    -- * Reexport
    -- | SetCookie(..)
    , module Web.Cookie
    -- | def
    , module Data.Default.Class
    ) where

import Web.Cookie (SetCookie(..))
import Data.Default.Class
import Web.Apiary.Cookie.Internal
