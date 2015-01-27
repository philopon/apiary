{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.Apiary.Filter.Internal
    ( function, function', function_, focus
    , Doc(..)
    ) where

import qualified Network.Wai as Wai

import Control.Monad(mzero)
import Control.Monad.Apiary.Internal(Filter, Filter', focus)
import Control.Monad.Apiary.Action(getRequest)

import Data.Apiary.Compat(KnownSymbol)
import Data.Apiary.Dict(Dict, type (</), KV((:=)))
import qualified Data.Apiary.Dict as Dict
import qualified Data.Apiary.Router as R
import Data.Apiary.Document.Internal(Doc(..))

-- | low level filter function.
function :: Monad actM => (Doc -> Doc)
         -> (Dict prms -> Wai.Request -> Maybe (Dict prms'))
         -> Filter exts actM m prms prms'
function doc f = focus doc Nothing $ R.raw "function" $ \d t -> do
    req <- getRequest
    case f d req of
        Nothing -> mzero
        Just c' -> return (c', t)

-- | filter and append argument.
function' :: (KnownSymbol key, Monad actM, key </ prms) => (Doc -> Doc) -> (Wai.Request -> Maybe (proxy key, prm))
          -> Filter exts actM m prms (key := prm ': prms)
function' d f = function d $ \c r -> f r >>= \(k, p) -> return $ Dict.add k p c

-- | filter only(not modify arguments).
function_ :: Monad actM => (Doc -> Doc) -> (Wai.Request -> Bool) 
          -> Filter' exts actM m
function_ d f = function d $ \c r -> if f r then Just c else Nothing
