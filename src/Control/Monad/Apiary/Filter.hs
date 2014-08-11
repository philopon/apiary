{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Apiary.Filter (
    -- * filters
    -- ** http method
      method, Method(..)
    -- ** http version
    , Control.Monad.Apiary.Filter.httpVersion
    , http09, http10, http11
    -- ** path matcher
    , root
    , anyPath
    , capture
    , Capture.path
    , Capture.endPath
    , Capture.fetch

    -- ** query matcher
    , QueryKey(..), (??)
    , query
    -- *** specified operators
    , (=:), (=!:), (=?:), (?:), (=*:), (=+:)
    , hasQuery

    -- ** header matcher
    , hasHeader
    , eqHeader
    , headers
    , header
    , header'

    -- ** other
    , ssl
    
    -- * deprecated
    , stdMethod

    ) where

import Network.Wai as Wai
import qualified Network.HTTP.Types as HT

import Control.Monad
import Control.Monad.Trans

import Control.Monad.Apiary.Action.Internal
import Control.Monad.Apiary.Filter.Internal
import Control.Monad.Apiary.Filter.Internal.Capture.TH
import Control.Monad.Apiary.Internal
import qualified Control.Monad.Apiary.Filter.Internal.Strategy as Strategy
import qualified Control.Monad.Apiary.Filter.Internal.Capture as Capture

import Text.Blaze.Html
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Reflection

import Data.Apiary.Param
import Data.Apiary.Document
import Data.Apiary.Method

-- | filter by HTTP method. since 0.1.0.0.
--
-- @
-- method GET      -- stdmethod
-- method \"HOGE\" -- non standard method
-- @
method :: Monad n => Method -> ApiaryT c n m a -> ApiaryT c n m a
method m = focus' (DocMethod m) (Just m) id return

{-# DEPRECATED stdMethod "use method" #-}
-- | filter by HTTP method using StdMethod. since 0.1.0.0.
stdMethod :: Monad n => Method -> ApiaryT c n m a -> ApiaryT c n m a
stdMethod = method

-- | filter by ssl accessed. since 0.1.0.0.
ssl :: Monad n => ApiaryT c n m a -> ApiaryT c n m a
ssl = function_ (DocPrecondition "SSL required") isSecure

-- | http version filter. since 0.5.0.0.
httpVersion :: Monad n => HT.HttpVersion -> Html -> ApiaryT c n m b -> ApiaryT c n m b
httpVersion v h = function_ (DocPrecondition h) $ (v ==) . Wai.httpVersion

-- | http/0.9 only accepted fiter. since 0.5.0.0.
http09 :: Monad n => ApiaryT c n m b -> ApiaryT c n m b
http09 = Control.Monad.Apiary.Filter.httpVersion HT.http09 "HTTP/0.9 only"

-- | http/1.0 only accepted fiter. since 0.5.0.0.
http10 :: Monad n => ApiaryT c n m b -> ApiaryT c n m b
http10 = Control.Monad.Apiary.Filter.httpVersion HT.http10 "HTTP/1.0 only"

-- | http/1.1 only accepted fiter. since 0.5.0.0.
http11 :: Monad n => ApiaryT c n m b -> ApiaryT c n m b
http11 = Control.Monad.Apiary.Filter.httpVersion HT.http11 "HTTP/1.1 only"

-- | filter by 'Control.Monad.Apiary.Action.rootPattern' of 'Control.Monad.Apiary.Action.ApiaryConfig'.
root :: (Functor m, Monad m, Monad n) => ApiaryT c n m b -> ApiaryT c n m b
root = focus' DocRoot Nothing (RootPath:) return

-- | match all subsequent path. since 0.15.0.
anyPath :: (Functor m, Monad m, Monad n) => ApiaryT c n m b -> ApiaryT c n m b
anyPath = focus' id Nothing (AnyPath:) return

--------------------------------------------------------------------------------

data QueryKey = QueryKey
    { queryKey  :: S.ByteString
    , queryDesc :: Maybe Html
    }

instance IsString QueryKey where
    fromString s = QueryKey (SC.pack s) Nothing

(??) :: QueryKey -> Html -> QueryKey
QueryKey k _ ?? d = QueryKey k (Just d)

-- | low level query getter. since 0.5.0.0.
--
-- @
-- query "key" (Proxy :: Proxy (fetcher type))
-- @
--
-- examples:
--
-- @
-- query "key" (Proxy :: Proxy ('First' Int)) -- get first \'key\' query parameter as Int.
-- query "key" (Proxy :: Proxy ('Option' (Maybe Int)) -- get first \'key\' query parameter as Int. allow without param or value.
-- query "key" (Proxy :: Proxy ('Many' String) -- get all \'key\' query parameter as String.
-- @
query :: forall a as w n m b proxy. 
      (ReqParam a, Strategy.Strategy w, MonadIO n)
      => QueryKey
      -> proxy (w a)
      -> ApiaryT (Strategy.SNext w as a) n m b
      -> ApiaryT as n m b
query QueryKey{..} p =
    focus doc $ \l -> do
        r     <- getRequest
        (q,f) <- getRequestBody

        maybe mzero return $
            Strategy.readStrategy id ((queryKey ==) . fst) p 
            (reqParams (Proxy :: Proxy a) r q f) l
  where
    doc = DocQuery queryKey (Strategy.strategyRep (Proxy :: Proxy w)) (reqParamRep (Proxy :: Proxy a)) queryDesc

-- | get first matched paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pFirst pInt) == query "key" (Proxy :: Proxy (First Int))
-- @
(=:) :: (MonadIO n, ReqParam a) => QueryKey -> proxy a 
     -> ApiaryT (a ': as) n m b -> ApiaryT as n m b
k =: t = query k (Strategy.pFirst t)

-- | get one matched paramerer. since 0.5.0.0.
--
-- when more one parameger given, not matched.
--
-- @
-- "key" =: pInt == query "key" (pOne pInt) == query "key" (Proxy :: Proxy (One Int))
-- @
(=!:) :: (MonadIO n, ReqParam a) => QueryKey -> proxy a 
      -> ApiaryT (a ': as) n m b -> ApiaryT as n m b
k =!: t = query k (Strategy.pOne t)

-- | get optional first paramerer. since 0.5.0.0.
--
-- when illegal type parameter given, fail mather(don't give Nothing).
--
-- @
-- "key" =: pInt == query "key" (pOption pInt) == query "key" (Proxy :: Proxy (Option Int))
-- @
(=?:) :: (MonadIO n, ReqParam a) => QueryKey -> proxy a 
      -> ApiaryT (Maybe a ': as) n m b -> ApiaryT as n m b
k =?: t = query k (Strategy.pOption t)

-- | check parameger given and type. since 0.5.0.0.
--
-- If you wan't to allow any type, give 'pVoid'.
--
-- @
-- "key" =: pInt == query "key" (pCheck pInt) == query "key" (Proxy :: Proxy (Check Int))
-- @
(?:) :: (MonadIO n, ReqParam a) => QueryKey -> proxy a 
     -> ApiaryT as n m b -> ApiaryT as n m b
k ?: t = query k (Strategy.pCheck t)

-- | get many paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pMany pInt) == query "key" (Proxy :: Proxy (Many Int))
-- @
(=*:) :: (MonadIO n, ReqParam a) => QueryKey -> proxy a 
      -> ApiaryT ([a] ': as) n m b -> ApiaryT as n m b
k =*: t = query k (Strategy.pMany t)

-- | get some paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pSome pInt) == query "key" (Proxy :: Proxy (Some Int))
-- @
(=+:) :: (MonadIO n, ReqParam a) => QueryKey -> proxy a 
      -> ApiaryT ([a] ': as) n m b -> ApiaryT as n m b
k =+: t = query k (Strategy.pSome t)

-- | query exists checker.
--
-- @
-- hasQuery q = 'query' q (Proxy :: Proxy ('Check' ()))
-- @
--
hasQuery :: (MonadIO n) => QueryKey -> ApiaryT c n m a -> ApiaryT c n m a
hasQuery q = query q (Proxy :: Proxy (Strategy.Check ()))

--------------------------------------------------------------------------------

-- | check whether to exists specified header or not. since 0.6.0.0.
hasHeader :: Monad n => HT.HeaderName -> ApiaryT as n m b -> ApiaryT as n m b
hasHeader n = header' Strategy.pCheck ((n ==) . fst) . Just $
    toHtml (show n) <> " header requred"

-- | check whether to exists specified valued header or not. since 0.6.0.0.
eqHeader :: Monad n
         => HT.HeaderName 
         -> S.ByteString  -- ^ header value
         -> ApiaryT as n m b
         -> ApiaryT as n m b
eqHeader k v = header' Strategy.pCheck (\(k',v') -> k == k' && v == v') . Just $
    mconcat [toHtml $ show k, " header == ", toHtml $ show v]

-- | filter by header and get first. since 0.6.0.0.
header :: Monad n => HT.HeaderName
       -> ApiaryT (S.ByteString ': as) n m b -> ApiaryT as n m b
header n = header' Strategy.pFirst ((n ==) . fst) . Just $
    toHtml (show n) <> " header requred"

-- | filter by headers up to 100 entries. since 0.6.0.0.
headers :: Monad n => HT.HeaderName
        -> ApiaryT ([S.ByteString] ': as) n m b -> ApiaryT as n m b
headers n = header' limit100 ((n ==) . fst) . Just $
    toHtml (show n) <> " header requred"
  where
    limit100 :: Proxy x -> Proxy (Strategy.LimitSome $(int 100) x)
    limit100 _ = Proxy

-- | low level header filter. since 0.6.0.0.
header' :: (Strategy.Strategy w, Monad n)
        => (forall x. Proxy x -> Proxy (w x))
        -> (HT.Header -> Bool)
        -> Maybe Html
        -> ApiaryT (Strategy.SNext w as S.ByteString) n m b
        -> ApiaryT as n m b
header' pf kf d = function pc $ \l r ->
    Strategy.readStrategy Just kf (pf pByteString) (requestHeaders r) l
  where
    pc = maybe id DocPrecondition d
