{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

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
function :: Monad actM => (Doc -> Doc)
         -> (SList prms -> Request -> Maybe (SList prms'))
         -> ApiaryT exts prms' actM m () -> ApiaryT exts prms actM m ()
function d f = focus d $ \c -> getRequest >>= \r -> case f c r of
    Nothing -> mzero
    Just c' -> return c'

-- | filter and append argument.
function' :: Monad actM => (Doc -> Doc) -> (Request -> Maybe prm)
          -> ApiaryT exts (prm ': prms) actM m () -> ApiaryT exts prms actM m ()
function' d f = function d $ \c r -> (::: c) `fmap` f r

-- | filter only(not modify arguments).
function_ :: Monad actM => (Doc -> Doc) -> (Request -> Bool) 
          -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
function_ d f = function d $ \c r -> if f r then Just c else Nothing
