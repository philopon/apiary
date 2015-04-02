{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Apiary.Filter.Capture
    ( path, fetch, fetch', anyPath, restPath
    ) where

import Control.Monad.Apiary.Internal (Filter, Filter', focus)
import Control.Monad.Apiary.Filter.Internal
    (Doc(DocPath, DocFetch, DocAny, DocRest))

import GHC.TypeLits.Compat(KnownSymbol, symbolVal)
import Data.Proxy.Compat(Proxy(..))
import Data.Apiary.Param(Path, pathRep, readPathAs)
import Network.Routing.Dict(KV((:=)))
import qualified Network.Routing.Dict as Dict
import qualified Network.Routing as R

import Data.Apiary.Document.Internal(Desc(..))
import qualified Data.Text as T

-- | check first path and drill down. since 0.11.0.
path :: Monad actM => T.Text -> Filter' exts actM m
path p = focus (DocPath p) Nothing (R.exact p)

-- | get first path and drill down. since 0.11.0.
fetch' :: (k Dict.</ prms, KnownSymbol k, Path p, Monad actM, Desc d)
       => proxy k -> proxy' p -> d
       -> Filter exts actM m prms (k ':= p ': prms)
fetch' k p h = focus (DocFetch (T.pack $ symbolVal k) (pathRep p) (toDesc h)) Nothing $ R.fetch k (readPathAs p)

fetch :: forall proxy k p d exts prms actM m. (k Dict.</ prms, KnownSymbol k, Path p, Monad actM, Desc d)
      => proxy (k ':= p) -> d
      -> Filter exts actM m prms (k ':= p ': prms)
fetch _ h = fetch' k p h
  where
    k = Proxy :: Proxy k
    p = Proxy :: Proxy p

anyPath :: (Monad m, Monad actM) => Filter' exts actM m
anyPath = focus DocAny Nothing R.any

restPath :: (k Dict.</ prms, KnownSymbol k, Monad m, Monad actM, Desc d)
         => proxy k -> d
         -> Filter exts actM m prms (k ':= [T.Text] ': prms)
restPath k h = focus (DocRest (T.pack $ symbolVal k) (toDesc h)) Nothing (R.rest k)
