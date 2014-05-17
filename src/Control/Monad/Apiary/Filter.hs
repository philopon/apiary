{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Apiary.Filter
    ( method, stdMethod, root
    , ssl
    , Control.Monad.Apiary.Filter.httpVersion
    , http09, http10, http11
    -- * query parameter
    -- ** query getter(always success)
    , queryMany, queryMany'
    , maybeQueryFirst, maybeQueryFirst'
    -- ** query filter
    , hasQuery
    , querySome, querySome'
    , queryFirst, queryFirst'
    -- * low level
    , function, function'
    -- * Reexport
    -- StdMethod(..)
    , module Network.HTTP.Types
    -- * deprecated
    , queryAll, queryAll'
    ) where

import Control.Monad
import Network.Wai as Wai
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types (StdMethod(..))
import qualified Data.ByteString as S
import Data.Maybe

import Data.Apiary.SList

import Control.Monad.Apiary.Action.Internal
import Control.Monad.Apiary.Internal

-- | raw and most generic filter function.
function :: Monad m => (SList c -> Request -> Maybe (SList c')) -> ApiaryT c' m b -> ApiaryT c m b
function f = focus $ \r c -> case f c r of
    Nothing -> mzero
    Just c' -> return c'

-- | filter and append argument.
function' :: Monad m => (Request -> Maybe a) -> ApiaryT (Snoc as a) m b -> ApiaryT as m b
function' f = function $ \c r -> sSnoc c `fmap` f r

-- | filter only(not modify arguments).
function_ :: Monad m => (Request -> Bool) -> ApiaryT c m b -> ApiaryT c m b
function_ f = function $ \c r -> if f r then Just c else Nothing

ssl :: Monad m => ApiaryT c m a -> ApiaryT c m a
ssl = function_ isSecure

-- | http version filter. since 0.4.4.0.
httpVersion :: Monad m => HT.HttpVersion  -> ApiaryT c m b -> ApiaryT c m b
httpVersion v = function_ $ (v ==) . Wai.httpVersion

http09 :: Monad m => ApiaryT c m b -> ApiaryT c m b
http09 = Control.Monad.Apiary.Filter.httpVersion HT.http09

http10 :: Monad m => ApiaryT c m b -> ApiaryT c m b
http10 = Control.Monad.Apiary.Filter.httpVersion HT.http10

http11 :: Monad m => ApiaryT c m b -> ApiaryT c m b
http11 = Control.Monad.Apiary.Filter.httpVersion HT.http11

-- | get [0,) parameters by query parameter allows empty value. since 0.4.3.0.
queryMany :: Monad m => S.ByteString
          -> ApiaryT (Snoc as [Maybe S.ByteString]) m b
          -> ApiaryT as m b
queryMany q = function' $ Just . map snd . filter ((q ==) . fst) . queryString

-- | filter [1,) parameters by query parameter allows empty value. since 0.4.3.0.
querySome :: Monad m => S.ByteString
          -> ApiaryT (Snoc as [Maybe S.ByteString]) m b
          -> ApiaryT as m b
querySome q = function' $ \r -> case map snd . filter ((q ==) . fst) $ queryString r of
    [] -> Nothing
    as -> Just as

-- | filter by query parameter. since 0.4.0.0.
queryAll :: Monad m => S.ByteString
         -> ApiaryT (Snoc as [Maybe S.ByteString]) m b -- ^ Nothing == no value paramator.
         -> ApiaryT as m b
queryAll = querySome
{-# DEPRECATED queryAll "use querySome" #-}

-- | get [0,) parameters by query parameter not allows empty value. since 0.4.3.0.
queryMany' :: Monad m => S.ByteString
           -> ApiaryT (Snoc as [S.ByteString]) m b 
           -> ApiaryT as m b
queryMany' q = function' $ Just . mapMaybe snd . filter ((q ==) . fst) . queryString

-- | filter [1,) parameters by query parameter not allows empty value. since 0.4.3.0.
querySome' :: Monad m => S.ByteString
           -> ApiaryT (Snoc as [S.ByteString]) m b 
           -> ApiaryT as m b
querySome' q = function' $ \r -> case mapMaybe snd . filter ((q ==) . fst) $ queryString r of
    [] -> Nothing
    as -> Just as

-- | filter by query parameter. since 0.4.0.0.
queryAll' :: Monad m => S.ByteString
          -> ApiaryT (Snoc as [S.ByteString]) m b 
          -> ApiaryT as m b
queryAll' = querySome'
{-# DEPRECATED queryAll' "use querySome'" #-}

-- | get first query parameter allow empty value. since 0.4.3.0,
maybeQueryFirst :: Monad m => S.ByteString
                -> ApiaryT (Snoc as (Maybe (Maybe S.ByteString))) m b
                -> ApiaryT as m b
maybeQueryFirst q = function' (Just . lookup q . queryString)

-- | filter by query parameter. allow empty value. since 0.4.0.0.
queryFirst :: Monad m => S.ByteString
           -> ApiaryT (Snoc as (Maybe S.ByteString)) m b
           -> ApiaryT as m b
queryFirst q = function' (lookup q . queryString)

-- | get first query parameter not allow empty value. since 0.4.3.0,
maybeQueryFirst' :: Monad m => S.ByteString
                 -> ApiaryT (Snoc as (Maybe S.ByteString)) m b
                 -> ApiaryT as m b
maybeQueryFirst' q = function' $ Just . listToMaybe . mapMaybe snd . filter ((q ==) . fst) . queryString

-- | filter by query parameter. not allow empty value. since 0.4.0.0.
queryFirst' :: Monad m => S.ByteString
            -> ApiaryT (Snoc as S.ByteString) m b
            -> ApiaryT as m b
queryFirst' q = function' $ listToMaybe . mapMaybe snd . filter ((q ==) . fst) . queryString

hasQuery :: Monad m => S.ByteString -> ApiaryT c m a -> ApiaryT c m a
hasQuery q = function_ (any ((q ==) . fst) . queryString)

method :: Monad m => HT.Method -> ApiaryT c m a -> ApiaryT c m a
method m = function_ ((m ==) . requestMethod)

stdMethod :: Monad m => StdMethod -> ApiaryT c m a -> ApiaryT c m a
stdMethod = method . HT.renderStdMethod

-- | filter by 'Control.Monad.Apiary.Action.rootPattern' of 'Control.Monad.Apiary.Action.ApiaryConfig'.
root :: Monad m => ApiaryT c m b -> ApiaryT c m b
root m = do
    rs <- rootPattern `liftM` apiaryConfig
    function_ (\r -> rawPathInfo r `elem` rs) m
