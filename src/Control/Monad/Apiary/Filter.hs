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
      method
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
    , (=:), (=!:), (=?:), (=?!:), (?:), (=*:), (=+:)
    , hasQuery

    -- ** header matcher
    , hasHeader
    , eqHeader
    , headers
    , header
    , header'
    , accept

    -- ** other
    , ssl
    
    -- * deprecated
    , stdMethod

    ) where

import Network.Wai as Wai
import Network.Wai.Parse
import qualified Network.HTTP.Types as HT

import Control.Applicative
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
import qualified Data.Text.Encoding as T
import Data.Monoid
import Data.Proxy
import Data.String

import Data.Apiary.Param
import Data.Apiary.Document
import Data.Apiary.Method

-- | filter by HTTP method. since 0.1.0.0.
--
-- @
-- method GET      -- stdmethod
-- method \"HOGE\" -- non standard method
-- @
method :: Monad actM => Method -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
method m = focus' (DocMethod m) (Just m) id return

{-# DEPRECATED stdMethod "use method" #-}
-- | filter by HTTP method using StdMethod. since 0.1.0.0.
stdMethod :: Monad actM => Method -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
stdMethod = method

-- | filter by ssl accessed. since 0.1.0.0.
ssl :: Monad actM => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
ssl = function_ (DocPrecondition "SSL required") isSecure

-- | http version filter. since 0.5.0.0.
httpVersion :: Monad actM => HT.HttpVersion -> Html -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
httpVersion v h = function_ (DocPrecondition h) $ (v ==) . Wai.httpVersion

-- | http/0.9 only accepted fiter. since 0.5.0.0.
http09 :: Monad actM => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
http09 = Control.Monad.Apiary.Filter.httpVersion HT.http09 "HTTP/0.9 only"

-- | http/1.0 only accepted fiter. since 0.5.0.0.
http10 :: Monad actM => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
http10 = Control.Monad.Apiary.Filter.httpVersion HT.http10 "HTTP/1.0 only"

-- | http/1.1 only accepted fiter. since 0.5.0.0.
http11 :: Monad actM => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
http11 = Control.Monad.Apiary.Filter.httpVersion HT.http11 "HTTP/1.1 only"

-- | filter by 'Control.Monad.Apiary.Action.rootPattern' of 'Control.Monad.Apiary.Action.ApiaryConfig'.
root :: (Monad m, Monad actM) => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
root = focus' DocRoot Nothing (RootPath:) return

-- | match all subsequent path. since 0.15.0.
anyPath :: (Monad m, Monad actM) => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
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
-- query "key" ('First'  :: First Int) -- get first \'key\' query parameter as Int.
-- query "key" ('Option' :: Option (Maybe Int)) -- get first \'key\' query parameter as Int. allow without param or value.
-- query "key" ('Many'   :: Many String) -- get all \'key\' query parameter as String.
-- @
query :: forall exts prms actM a w m. 
      (ReqParam a, Strategy.Strategy w, MonadIO actM)
      => QueryKey
      -> w a
      -> ApiaryT exts (Strategy.SNext w prms a) actM m ()
      -> ApiaryT exts prms actM m ()
query QueryKey{..} p =
    focus doc $ \l -> do
        r     <- getRequest
        (q,f) <- getRequestBody

        maybe mzero return $
            Strategy.readStrategy id ((queryKey ==) . fst) p 
            (reqParams p r q f) l
  where
    doc = DocQuery queryKey (Strategy.strategyRep p) (reqParamRep (Proxy :: Proxy a)) queryDesc

-- | get first matched paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pFirst pInt) == query "key" (First :: First Int)
-- @
(=:) :: (MonadIO actM, ReqParam p) => QueryKey -> proxy p
     -> ApiaryT exts (p ': prms) actM m () -> ApiaryT exts prms actM m ()
k =: t = query k (Strategy.pFirst t)

-- | get one matched paramerer. since 0.5.0.0.
--
-- when more one parameger given, not matched.
--
-- @
-- "key" =: pInt == query "key" (pOne pInt) == query "key" (One :: One Int)
-- @
(=!:) :: (MonadIO actM, ReqParam p) => QueryKey -> proxy p
      -> ApiaryT exts (p ': prms) actM m () -> ApiaryT exts prms actM m ()
k =!: t = query k (Strategy.pOne t)

-- | get optional first paramerer. since 0.5.0.0.
--
-- when illegal type parameter given, fail match(don't give Nothing).
--
-- @
-- "key" =: pInt == query "key" (pOption pInt) == query "key" (Option :: Option Int)
-- @
(=?:) :: (MonadIO actM, ReqParam p) => QueryKey -> proxy p
      -> ApiaryT exts (Maybe p ': prms) actM m () -> ApiaryT exts prms actM m ()
k =?: t = query k (Strategy.pOption t)

-- | get optional first paramerer with default. since 0.16.0.
--
-- when illegal type parameter given, fail match(don't give Nothing).
--
-- @
-- "key" =: (0 :: Int) == query "key" (pOptional (0 :: Int)) == query "key" (Optional 0 :: Optional Int)
-- @
(=?!:) :: (MonadIO actM, ReqParam p, Show p) => QueryKey -> p
       -> ApiaryT exts (p ': prms) actM m () -> ApiaryT exts prms actM m ()
k =?!: v = query k (Strategy.pOptional v)

-- | check parameger given and type. since 0.5.0.0.
--
-- If you wan't to allow any type, give 'pVoid'.
--
-- @
-- "key" =: pInt == query "key" (pCheck pInt) == query "key" (Check :: Check Int)
-- @
(?:) :: (MonadIO actM, ReqParam p) => QueryKey -> proxy p
     -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
k ?: t = query k (Strategy.pCheck t)

-- | get many paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pMany pInt) == query "key" (Many :: Many Int)
-- @
(=*:) :: (MonadIO actM, ReqParam p) => QueryKey -> proxy p
      -> ApiaryT exts ([p] ': prms) actM m () -> ApiaryT exts prms actM m ()
k =*: t = query k (Strategy.pMany t)

-- | get some paramerer. since 0.5.0.0.
--
-- @
-- "key" =: pInt == query "key" (pSome pInt) == query "key" (Some :: Some Int)
-- @
(=+:) :: (MonadIO actM, ReqParam p) => QueryKey -> proxy p
      -> ApiaryT exts ([p] ': prms) actM m () -> ApiaryT exts prms actM m ()
k =+: t = query k (Strategy.pSome t)

-- | query exists checker.
--
-- @
-- hasQuery q = 'query' q (Check :: 'Check' ())
-- @
--
hasQuery :: MonadIO actM => QueryKey -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
hasQuery q = query q (Strategy.Check :: Strategy.Check ())

--------------------------------------------------------------------------------

-- | check whether to exists specified header or not. since 0.6.0.0.
hasHeader :: Monad actM => HT.HeaderName -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
hasHeader n = header' Strategy.pCheck ((n ==) . fst) . Just $
    toHtml (show n) <> " header requred"

-- | check whether to exists specified valued header or not. since 0.6.0.0.
eqHeader :: Monad actM
         => HT.HeaderName 
         -> S.ByteString  -- ^ header value
         -> ApiaryT exts prms actM m ()
         -> ApiaryT exts prms actM m ()
eqHeader k v = header' Strategy.pCheck (\(k',v') -> k == k' && v == v') . Just $
    mconcat [toHtml $ show k, " header == ", toHtml $ show v]

-- | filter by header and get first. since 0.6.0.0.
header :: Monad actM => HT.HeaderName
       -> ApiaryT exts (S.ByteString ': prms) actM m () -> ApiaryT exts prms actM m ()
header n = header' Strategy.pFirst ((n ==) . fst) . Just $
    toHtml (show n) <> " header requred"

-- | filter by headers up to 100 entries. since 0.6.0.0.
headers :: Monad actM => HT.HeaderName
        -> ApiaryT exts ([S.ByteString] ': prms) actM m () -> ApiaryT exts prms actM m ()
headers n = header' (Strategy.pLimitSome 100) ((n ==) . fst) . Just $
    toHtml (show n) <> " header requred"

-- | low level header filter. since 0.6.0.0.
header' :: (Strategy.Strategy w, Monad actM)
        => (forall x. Proxy x -> w x)
        -> (HT.Header -> Bool)
        -> Maybe Html
        -> ApiaryT exts (Strategy.SNext w prms S.ByteString) actM m ()
        -> ApiaryT exts prms actM m ()
header' pf kf d = function pc $ \l r ->
    Strategy.readStrategy Just kf (pf pByteString) (requestHeaders r) l
  where
    pc = maybe id DocPrecondition d

-- | require Accept header and set response Content-Type. since 0.16.0.
accept :: Monad actM => ContentType -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
accept ect = focus (DocPrecondition $ "Accept: " <> toHtml (T.decodeUtf8 ect)) $ \c ->
    (lookup "Accept" . requestHeaders <$> getRequest) >>= \case
        Nothing -> mzero
        Just ac -> if parseContentType ect `elem` map (parseContentType . SC.dropWhile (== ' ')) (SC.split ',' ac)
                   then contentType ect >> return c
                   else mzero
