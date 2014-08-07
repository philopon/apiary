module Control.Monad.Apiary.Filter.Internal
    ( function, function', function_, focus
    ) where

import Network.Wai

import Control.Monad
import Control.Monad.Apiary.Internal
import Control.Monad.Apiary.Action

import Data.Apiary.SList
import Data.Apiary.Document

-- | low level filter function.
function :: Monad n => (Doc -> Doc)
         -> (SList c -> Request -> Maybe (SList c'))
         -> ApiaryT c' n m b -> ApiaryT c n m b
function d f = focus d $ \c -> getRequest >>= \r -> case f c r of
    Nothing -> mzero
    Just c' -> return c'

-- | filter and append argument.
function' :: Monad n => (Doc -> Doc) -> (Request -> Maybe a)
          -> ApiaryT (Snoc as a) n m b -> ApiaryT as n m b
function' d f = function d $ \c r -> sSnoc c `fmap` f r

-- | filter only(not modify arguments).
function_ :: Monad n => (Doc -> Doc) -> (Request -> Bool) 
          -> ApiaryT c n m b -> ApiaryT c n m b
function_ d f = function d $ \c r -> if f r then Just c else Nothing
