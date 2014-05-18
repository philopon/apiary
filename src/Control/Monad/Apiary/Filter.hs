{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Apiary.Filter (
    -- * filters
    -- ** http method
      method, stdMethod
    -- ** http version
    , Control.Monad.Apiary.Filter.httpVersion
    , http09, http10, http11
    -- ** path matcher
    , root
    , capture

    -- ** query matcher
    , query
    , pFirst, pOne, pOption, pCheck, pMany, pSome
    -- *** specified operators
    , (=:), (=!:), (=?:), (?:), (=*:), (=+:)
    , hasQuery


    -- ** other
    , ssl
    
    -- * Reexport
    -- StdMethod(..)
    , module Network.HTTP.Types
    -- * deprecated
    , queryAll,        queryAll'
    , querySome,       querySome'
    , queryFirst,      queryFirst'
    , queryMany,       queryMany'
    , maybeQueryFirst, maybeQueryFirst'
    ) where

import Control.Monad
import Network.Wai as Wai
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types (StdMethod(..))
import qualified Data.ByteString as S
import Data.Maybe
import Data.Proxy

import Data.Apiary.SList
import Data.Apiary.Param

import Control.Monad.Apiary.Action.Internal
import Control.Monad.Apiary.Filter.Internal
import Control.Monad.Apiary.Filter.Internal.Query
import Control.Monad.Apiary.Filter.Internal.Capture.TH
import Control.Monad.Apiary.Internal

ssl :: Monad m => ApiaryT c m a -> ApiaryT c m a
ssl = function_ isSecure

-- | http version filter. since 0.5.0.0.
httpVersion :: Monad m => HT.HttpVersion  -> ApiaryT c m b -> ApiaryT c m b
httpVersion v = function_ $ (v ==) . Wai.httpVersion

-- | http/0.9 only accepted fiter. since 0.5.0.0.
http09 :: Monad m => ApiaryT c m b -> ApiaryT c m b
http09 = Control.Monad.Apiary.Filter.httpVersion HT.http09

-- | http/1.0 only accepted fiter. since 0.5.0.0.
http10 :: Monad m => ApiaryT c m b -> ApiaryT c m b
http10 = Control.Monad.Apiary.Filter.httpVersion HT.http10

-- | http/1.1 only accepted fiter. since 0.5.0.0.
http11 :: Monad m => ApiaryT c m b -> ApiaryT c m b
http11 = Control.Monad.Apiary.Filter.httpVersion HT.http11

method :: Monad m => HT.Method -> ApiaryT c m a -> ApiaryT c m a
method m = function_ ((m ==) . requestMethod)

stdMethod :: Monad m => StdMethod -> ApiaryT c m a -> ApiaryT c m a
stdMethod = method . HT.renderStdMethod

-- | filter by 'Control.Monad.Apiary.Action.rootPattern' of 'Control.Monad.Apiary.Action.ApiaryConfig'.
root :: Monad m => ApiaryT c m b -> ApiaryT c m b
root m = do
    rs <- rootPattern `liftM` apiaryConfig
    function_ (\r -> rawPathInfo r `elem` rs) m

-- | get first matched paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pFirst pInt) == query "key" (Proxy :: Proxy (First Int))
-- @
(=:) :: (Query a, Monad m)
     => S.ByteString -> Proxy a -> ApiaryT (Snoc as a) m b -> ApiaryT as m b
k =: t = query k (pFirst t)

-- | get one matched paramerer. since 0.5.0.0.
--
-- when more one parameger given, not matched.
--
-- @
-- "key" =: pInt == query "key" (pOne pInt) == query "key" (Proxy :: Proxy (One Int))
-- @
(=!:) :: (Query a, Monad m)
      => S.ByteString -> Proxy a -> ApiaryT (Snoc as a) m b -> ApiaryT as m b
k =!: t = query k (pOne t)

-- | get optional first paramerer. since 0.5.0.0.
--
-- when illegal type parameter given, fail mather(don't give Nothing).
--
-- @
-- "key" =: pInt == query "key" (pOption pInt) == query "key" (Proxy :: Proxy (Option Int))
-- @
(=?:) :: (Query a, Monad m)
      => S.ByteString -> Proxy a -> ApiaryT (Snoc as (Maybe a)) m b -> ApiaryT as m b
k =?: t = query k (pOption t)

-- | check parameger given and type. since 0.5.0.0.
--
-- If you wan't to allow any type, give 'pVoid'.
--
-- @
-- "key" =: pInt == query "key" (pCheck pInt) == query "key" (Proxy :: Proxy (Check Int))
-- @
(?:) :: (Query a, Monad m)
     => S.ByteString -> Proxy a -> ApiaryT as m b -> ApiaryT as m b
k ?: t = query k (pCheck t)

-- | get many paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pMany pInt) == query "key" (Proxy :: Proxy (Many Int))
-- @
(=*:) :: (Query a, Monad m)
      => S.ByteString -> Proxy a -> ApiaryT (Snoc as [a]) m b -> ApiaryT as m b
k =*: t = query k (pMany t)

-- | get some paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pSome pInt) == query "key" (Proxy :: Proxy (Some Int))
-- @
(=+:) :: (Query a, Monad m)
      => S.ByteString -> Proxy a -> ApiaryT (Snoc as [a]) m b -> ApiaryT as m b
k =+: t = query k (pSome t)

-- | query exists checker.
--
-- @
-- hasQuery q = 'query' q (Proxy :: Proxy ('Check' ()))
-- @
--
hasQuery :: Monad m => S.ByteString -> ApiaryT c m a -> ApiaryT c m a
hasQuery q = query q (Proxy :: Proxy (Check ()))

--------------------------------------------------------------------------------

{-# DEPRECATED queryMany, querySome, queryAll, queryMany', querySome', queryAll'
  , maybeQueryFirst, queryFirst, maybeQueryFirst'
  , queryFirst' "use query related filters" #-}

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
