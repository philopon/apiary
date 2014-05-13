{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.Apiary.Filter
    ( method, stdMethod, root
    , ssl, hasQuery
    , function, function'
    -- * Reexport
    , StdMethod(..)
    ) where

import Control.Monad
import Network.Wai
import Network.HTTP.Types
import qualified Data.ByteString as S

import Control.Monad.Apiary.Action.Internal
import Control.Monad.Apiary.Internal

-- | raw filter function.
function :: Monad m => (c -> Request -> Maybe c') -> ApiaryT c' m a -> ApiaryT c m a
function f = focus $ \c -> getRequest >>= \r -> case f c r of
    Nothing -> mzero
    Just c' -> return c'

function' :: Monad m => (Request -> Bool) -> ApiaryT c m a -> ApiaryT c m a
function' f = function $ \c r -> if f r then Just c else Nothing

ssl :: Monad m => ApiaryT c m a -> ApiaryT c m a
ssl = function' isSecure

hasQuery :: Monad m => S.ByteString -> ApiaryT c m a -> ApiaryT c m a
hasQuery q = function' (any ((q ==) . fst) . queryString)

method :: Monad m => Method -> ApiaryT c m a -> ApiaryT c m a
method m = function' $ ((m ==) . requestMethod)

stdMethod :: Monad m => StdMethod -> ApiaryT c m a -> ApiaryT c m a
stdMethod = method . renderStdMethod

-- | filter by 'Control.Monad.Apiary.Action.rootPattern' of 'Control.Monad.Apiary.Action.ApiaryConfig'.
root :: Monad m => ApiaryT c m b -> ApiaryT c m b
root m = do
    rs <- rootPattern `liftM` apiaryConfig
    function (\c r -> if rawPathInfo r `elem` rs then Just c else Nothing) m

