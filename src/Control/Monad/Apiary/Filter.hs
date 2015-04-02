{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Apiary.Filter (
    Filter, Filter'
    -- * http method
    , method
    -- * http version
    , http09, http10, http11
    -- * path matcher
    , root
    , capture
    -- * query matcher
    , (??)
    , (=:), (=!:), (=?:), (=?!:), (=*:), (=+:)
    , switchQuery

    -- * header matcher
    , eqHeader
    , header
    , accept

    -- * other
    , ssl

    -- * not export from Web.Apiary
    , HasDesc(..)
    , QueryKey(..)
    , query
    , Control.Monad.Apiary.Filter.httpVersion

    , function, function', function_, focus
    ) where

import qualified Network.Wai as Wai
import Network.Wai.Parse (parseContentType, parseHttpAccept)
import qualified Network.HTTP.Types as HTTP

import Control.Monad(mzero)
import Control.Monad.Trans(MonadIO)

import Control.Monad.Apiary.Action.Internal
    ( getQueryParams, getReqBodyParams
    , getRequest, ContentType, contentType
    , getConfig, ApiaryConfig(..)
    )

import Control.Monad.Apiary.Filter.Internal
    ( function, function', function_
    , Doc(DocMethod, DocPrecondition, DocRoot, DocQuery, DocAccept)
    )

import Control.Monad.Apiary.Filter.Internal.Capture.TH(capture)
import Control.Monad.Apiary.Internal(Filter, Filter', focus)

import qualified Data.ByteString.Char8 as SC
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import qualified Data.CaseInsensitive  as CI
import Data.Monoid((<>))
import Data.Proxy.Compat(Proxy(..))
import GHC.TypeLits.Compat(KnownSymbol, Symbol, symbolVal)
import Data.Apiary.SProxy(SProxy(..))
import Network.Routing.Dict(type (</), KV((:=)))
import qualified Network.Routing.Dict as Dict
import qualified Network.Routing as R

import Data.Apiary.Param
    ( StrategyRep(..), QueryRep(NoValue), Query(..)
    , Strategy(..), pFirst, pOne, pOption, pOptional, pMany, pSome
    )
import Data.Apiary.Method(Method)

-- | filter by HTTP method. since 0.1.0.0.
--
-- @
-- method GET      -- stdmethod
-- method \"HOGE\" -- non standard method
-- @
method :: Monad actM => Method -> Filter' exts actM m
method m = focus (DocMethod m) (Just m) id

-- | filter by ssl accessed. since 0.1.0.0.
ssl :: Monad actM => Filter' exts actM m
ssl = function_ (DocPrecondition "SSL required") Wai.isSecure

-- | http version filter. since 0.5.0.0.
httpVersion :: Monad actM => HTTP.HttpVersion -> Html -> Filter' exts actM m
httpVersion v h = function_ (DocPrecondition h) $ (v ==) . Wai.httpVersion

-- | http/0.9 only accepted fiter. since 0.5.0.0.
http09 :: Monad actM => Filter' exts actM m
http09 = Control.Monad.Apiary.Filter.httpVersion HTTP.http09 "HTTP/0.9 only"

-- | http/1.0 only accepted fiter. since 0.5.0.0.
http10 :: Monad actM => Filter' exts actM m
http10 = Control.Monad.Apiary.Filter.httpVersion HTTP.http10 "HTTP/1.0 only"

-- | http/1.1 only accepted fiter. since 0.5.0.0.
http11 :: Monad actM => Filter' exts actM m
http11 = Control.Monad.Apiary.Filter.httpVersion HTTP.http11 "HTTP/1.1 only"

-- | filter by 'Control.Monad.Apiary.Action.rootPattern' of 'Control.Monad.Apiary.Action.ApiaryConfig'.
root :: (Monad m, Monad actM) => Filter' exts actM m
root = focus DocRoot Nothing $ R.raw "ROOT" $ \d r -> do
    roots <- rootPattern `fmap` getConfig
    case r of
        [] -> return (d, [])
        [p] | p `elem` roots -> return (d, [])
        _  -> mzero

--------------------------------------------------------------------------------

newtype QueryKey (key :: Symbol) = QueryKey { queryKeyDesc :: Maybe Html }

-- | add document to query parameter filter.
--
-- > [key|key|] ?? "document" =: pInt
--
(??) :: proxy key -> Html -> QueryKey key
_ ?? d = QueryKey (Just d)

class HasDesc (a :: Symbol -> *) where
    queryDesc :: a key -> Maybe Html

instance HasDesc QueryKey where
    queryDesc = queryKeyDesc

instance HasDesc Proxy where
    queryDesc = const Nothing

instance HasDesc SProxy where
    queryDesc = const Nothing

isRequestBodyMethod :: SC.ByteString -> Bool
isRequestBodyMethod m = m == "POST" || m == "PUT"

query :: forall query strategy k v exts prms actM m. (k </ prms, MonadIO actM, KnownSymbol k, Query v, HasDesc query, Strategy strategy)
      => query k -> strategy v -> Filter exts actM m prms (SNext strategy k v prms)
query k w = focus doc Nothing $ R.raw "query" $ \d t -> do
    meth <- Wai.requestMethod `fmap` getRequest
    ps <- if isRequestBodyMethod meth
          then map (fmap Just) `fmap` getReqBodyParams
          else getQueryParams
    case strategy w k (map (readQuery . snd) $ filter ((SC.pack (symbolVal k) ==) . fst) ps) d of
        Nothing -> mzero
        Just d' -> return (d', t)
  where
    doc = DocQuery (T.pack $ symbolVal k) (strategyRep w) (queryRep (Proxy :: Proxy v)) (queryDesc k)

-- | get first matched paramerer. since 0.5.0.0.
--
-- @
-- [key|key|] =: pInt
-- @
(=:) :: (HasDesc query, MonadIO actM, KnownSymbol k, Query v, k </ prms)
     => query k -> proxy v -> Filter exts actM m prms (k ':= v ': prms)
k =: v = query k (pFirst v)

-- | get one matched paramerer. since 0.5.0.0.
--
-- when more one parameger given, not matched.
--
-- @
-- [key|key|] =!: pInt
-- @
(=!:) :: (HasDesc query, MonadIO actM, KnownSymbol k, Query v, k </ prms)
      => query k -> proxy v -> Filter exts actM m prms (k ':= v ': prms)
k =!: t = query k (pOne t)

-- | get optional first paramerer. since 0.5.0.0.
--
-- when illegal type parameter given, fail match(don't give Nothing).
--
-- @
-- [key|key|] =?: pInt
-- @
(=?:) :: (HasDesc query, MonadIO actM, KnownSymbol k, Query v, k </ prms)
      => query k -> proxy v -> Filter exts actM m prms (k ':= Maybe v ': prms)
k =?: t = query k (pOption t)

-- | get optional first paramerer with default. since 0.16.0.
--
-- when illegal type parameter given, fail match(don't give Nothing).
--
-- @
-- [key|key|] =!?: (0 :: Int)
-- @
(=?!:) :: forall query k v exts prms actM m. (HasDesc query, MonadIO actM, Show v, KnownSymbol k, Query v, k </ prms)
       => query k -> v -> Filter exts actM m prms (k ':= v ': prms)
k =?!: v = query k (pOptional v)

-- | get many paramerer. since 0.5.0.0.
--
-- @
-- [key|key|] =*: pInt
-- @
(=*:) :: (HasDesc query, MonadIO actM, KnownSymbol k, Query v, k </ prms)
      => query k -> proxy v -> Filter exts actM m prms (k ':= [v] ': prms)
k =*: t = query k (pMany t)

-- | get some paramerer. since 0.5.0.0.
--
-- @
-- [key|key|] =+: pInt
-- @
(=+:) :: (HasDesc query, MonadIO actM, KnownSymbol k, Query v, k </ prms)
      => query k -> proxy v -> Filter exts actM m prms (k ':= [v] ': prms)
k =+: t = query k (pSome t)

-- | get existance of key only query parameter. since v0.17.0.
switchQuery :: (HasDesc proxy, MonadIO actM, KnownSymbol k, k </ prms)
            => proxy k -> Filter exts actM m prms (k ':= Bool ': prms)
switchQuery k = focus doc Nothing $ R.raw "switch" $ \d t -> do
    meth <- Wai.requestMethod `fmap` getRequest
    ps <- if isRequestBodyMethod meth
          then map (fmap Just) `fmap` getReqBodyParams
          else getQueryParams
    let n = maybe False id . fmap (maybe True id . readQuery) $ lookup (SC.pack $ symbolVal k) ps
    return (Dict.add k n d, t)
  where
    doc = (DocQuery (T.pack $ symbolVal k) (StrategyRep "switch") NoValue (queryDesc k))

--------------------------------------------------------------------------------

-- | filter by header and get first. since 0.6.0.0.
header :: (KnownSymbol k, Monad actM, k </ prms)
       => proxy k -> Filter exts actM m prms (k ':= SC.ByteString ': prms)
header k = focus doc Nothing $ R.raw "header" $ \d t -> do
    n <- maybe mzero return . lookup (CI.mk . SC.pack $ symbolVal k) . Wai.requestHeaders =<< getRequest
    return (Dict.add k n d, t)
  where
    doc = DocPrecondition $ "has header: " <> toHtml (TL.pack $ symbolVal k)

-- | check whether to exists specified valued header or not. since 0.6.0.0.
eqHeader :: (KnownSymbol k, Monad actM)
         => proxy k -> SC.ByteString -> Filter' exts actM m
eqHeader k v = focus doc Nothing $ R.raw "=header" $ \d t -> do
    v' <- maybe mzero return . lookup (CI.mk . SC.pack $ symbolVal k) . Wai.requestHeaders =<< getRequest
    if v == v' then return (d,t) else mzero
  where
    doc = DocPrecondition $ "header: " <> toHtml (TL.pack $ symbolVal k) <> " = " <> toHtml (TL.pack $ show v)

-- | require Accept header and set response Content-Type. since 0.16.0.
accept :: Monad actM => ContentType -> Filter' exts actM m
accept ect = focus (DocAccept ect) Nothing $ R.raw "accept" $ \d t ->
    fmap (lookup "Accept" . Wai.requestHeaders) getRequest >>= \case
        Nothing -> mzero
        Just ac ->
            let ex@(et, _) = parseContentType ect
                accepts    = map parseContentType (parseHttpAccept ac)
            in case filter (matchContentType ex) accepts of
                [] -> mzero
                (_,p):_ -> contentType (prettyContentType et p) >> return (d, t)

matchContentType :: (SC.ByteString, [(SC.ByteString, SC.ByteString)])
                 -> (SC.ByteString, [(SC.ByteString, SC.ByteString)])
                 -> Bool
matchContentType (ct, ep) (acc, ip) = case SC.break (== '/') acc of
    ("*", "/*") -> prmCheck
    (a,   "/*") -> a == SC.takeWhile (/= '/') ct && prmCheck
    _           -> acc == ct && prmCheck
  where
    prmCheck = all (\(k,v) -> Just v == lookup k ip) ep

prettyContentType :: SC.ByteString -> [(SC.ByteString, SC.ByteString)] -> SC.ByteString
prettyContentType ct prms =
    let pprms = SC.concat $ concatMap (\(k,v) -> [";", k, "=", v]) prms
    in ct `SC.append` pprms

