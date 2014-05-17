module Control.Monad.Apiary.Filter.Internal where

import Control.Monad
import Control.Monad.Apiary.Internal
import Network.Wai
import Data.Apiary.SList

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


