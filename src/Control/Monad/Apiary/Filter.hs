{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Apiary.Filter
    ( method, stdMethod, root
    , ssl
    , hasQuery
    , queryAll, queryAll', queryFirst, queryFirst'
    , function, function'
    -- * Reexport
    -- StdMethod(..)
    , module Network.HTTP.Types
    ) where

import Control.Monad
import Network.Wai
import qualified Network.HTTP.Types as Use
import Network.HTTP.Types (StdMethod(..))
import qualified Data.ByteString as S
import Data.Maybe

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

-- | filter by query parameter. since 0.4.0.0.
queryAll :: Monad m => S.ByteString
         -> ApiaryT (Snoc as [Maybe S.ByteString]) m b -- ^ Nothing == no value paramator.
         -> ApiaryT as m b
queryAll q = function' $ \r -> case filter ((q ==) . fst) $ queryString r of
    [] -> Nothing
    as -> Just $ map snd as

-- | filter by query parameter. since 0.4.0.0.
queryAll' :: Monad m => S.ByteString
          -> ApiaryT (Snoc as [S.ByteString]) m b 
          -> ApiaryT as m b
queryAll' q = function' $ \r -> case mapMaybe snd . filter ((q ==) . fst) $ queryString r of
    [] -> Nothing
    as -> Just as

-- | filter by query parameter. since 0.4.0.0.
queryFirst :: Monad m => S.ByteString
           -> ApiaryT (Snoc as (Maybe S.ByteString)) m b
           -> ApiaryT as m b
queryFirst q = function' (lookup q . queryString)

-- | filter by query parameter. since 0.4.0.0.
queryFirst' :: Monad m => S.ByteString
            -> ApiaryT (Snoc as S.ByteString) m b
            -> ApiaryT as m b
queryFirst' q = function' $ \r -> case mapMaybe snd . filter ((q ==) . fst) $ queryString r of
    []  -> Nothing
    a:_ -> Just a

hasQuery :: Monad m => S.ByteString -> ApiaryT c m a -> ApiaryT c m a
hasQuery q = function_ (any ((q ==) . fst) . queryString)

method :: Monad m => Use.Method -> ApiaryT c m a -> ApiaryT c m a
method m = function_ ((m ==) . requestMethod)

stdMethod :: Monad m => StdMethod -> ApiaryT c m a -> ApiaryT c m a
stdMethod = method . Use.renderStdMethod

-- | filter by 'Control.Monad.Apiary.Action.rootPattern' of 'Control.Monad.Apiary.Action.ApiaryConfig'.
root :: Monad m => ApiaryT c m b -> ApiaryT c m b
root m = do
    rs <- rootPattern `liftM` apiaryConfig
    function_ (\r -> rawPathInfo r `elem` rs) m
