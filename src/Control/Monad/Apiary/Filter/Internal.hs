module Control.Monad.Apiary.Filter.Internal
    ( function, function', function_, focus
    ) where

import Control.Monad
import Control.Monad.Apiary.Internal
import Control.Monad.Apiary.Action
import Network.Wai
import Data.Apiary.SList

-- | low level filter function.
function :: Monad n => (SList c -> Request -> Maybe (SList c'))
         -> ApiaryT c' n m b -> ApiaryT c n m b
function f = focus $ \c -> getRequest >>= \r -> case f c r of
    Nothing -> mzero
    Just c' -> return c'

-- | filter and append argument.
function' :: Monad n => (Request -> Maybe a)
          -> ApiaryT (Snoc as a) n m b -> ApiaryT as n m b
function' f = function $ \c r -> sSnoc c `fmap` f r

-- | filter only(not modify arguments).
function_ :: Monad n => (Request -> Bool) 
          -> ApiaryT c n m b -> ApiaryT c n m b
function_ f = function $ \c r -> if f r then Just c else Nothing


