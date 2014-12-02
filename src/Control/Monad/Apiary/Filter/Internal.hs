{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.Apiary.Filter.Internal
    ( function, function', function_, focus
    , Doc(..)
    ) where

import qualified Network.Wai as Wai

import Control.Monad(mzero)
import Control.Monad.Apiary.Internal(ApiaryT, focus)
import Control.Monad.Apiary.Action(getParams, getRequest)

import Data.Apiary.Compat(KnownSymbol)
import Data.Apiary.Dict(Dict, NotMember, Elem((:=)))
import qualified Data.Apiary.Dict as Dict
import Data.Apiary.Document.Internal(Doc(..))

-- | low level filter function.
function :: Monad actM => (Doc -> Doc)
         -> (Dict prms -> Wai.Request -> Maybe (Dict prms'))
         -> ApiaryT exts prms' actM m () -> ApiaryT exts prms actM m ()
function d f = focus d $ getParams >>= \p -> getRequest >>= \r -> case f p r of
    Nothing -> mzero
    Just c' -> return c'

-- | filter and append argument.
function' :: (KnownSymbol key, Monad actM, NotMember key prms) => (Doc -> Doc) -> (Wai.Request -> Maybe (proxy key, prm))
          -> ApiaryT exts (key := prm ': prms) actM m () -> ApiaryT exts prms actM m ()
function' d f = function d $ \c r -> f r >>= \(k, p) -> return $ Dict.insert k p c

-- | filter only(not modify arguments).
function_ :: Monad actM => (Doc -> Doc) -> (Wai.Request -> Bool) 
          -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
function_ d f = function d $ \c r -> if f r then Just c else Nothing
