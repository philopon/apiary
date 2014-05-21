module Control.Monad.Apiary.Filter.Internal
    ( function, function', function_, focus
    ) where

import Control.Monad
import Control.Monad.Apiary.Internal
import Control.Monad.Apiary.Action
import Network.Wai
import Data.Apiary.SList

-- | low level filter function.
function :: (SList c -> Request -> Maybe (SList c')) -> Apiary c' b -> Apiary c b
function f = focus $ \c -> getRequest >>= \r -> case f c r of
    Nothing -> mzero
    Just c' -> return c'

-- | filter and append argument.
function' :: (Request -> Maybe a) -> Apiary (Snoc as a) b -> Apiary as b
function' f = function $ \c r -> sSnoc c `fmap` f r

-- | filter only(not modify arguments).
function_ :: (Request -> Bool) -> Apiary c b -> Apiary c b
function_ f = function $ \c r -> if f r then Just c else Nothing
