{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.Apiary.Filter.Internal
    ( function, function', function_, focus
    , Doc(..)
    ) where

import Network.Wai

import Control.Monad
import Control.Monad.Apiary.Internal
import Control.Monad.Apiary.Action

import Data.Apiary.Compat
import Data.Apiary.Dict
import Data.Apiary.Document.Internal

-- | low level filter function.
function :: Monad actM => (Doc -> Doc)
         -> (Dict prms -> Request -> Maybe (Dict prms'))
         -> ApiaryT exts prms' actM m () -> ApiaryT exts prms actM m ()
function d f = focus d $ getParams >>= \p -> getRequest >>= \r -> case f p r of
    Nothing -> mzero
    Just c' -> return c'

-- | filter and append argument.
function' :: (KnownSymbol key, Monad actM, NotMember key prms) => (Doc -> Doc) -> (Request -> Maybe (proxy key, prm))
          -> ApiaryT exts (key := prm ': prms) actM m () -> ApiaryT exts prms actM m ()
function' d f = function d $ \c r -> f r >>= \(k, p) -> return $ insert k p c

-- | filter only(not modify arguments).
function_ :: Monad actM => (Doc -> Doc) -> (Request -> Bool) 
          -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
function_ d f = function d $ \c r -> if f r then Just c else Nothing
