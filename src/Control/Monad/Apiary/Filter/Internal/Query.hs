{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.Apiary.Filter.Internal.Query where

import Control.Monad.Apiary
import Control.Monad.Apiary.Filter.Internal
import Data.Apiary.Param
import Data.Apiary.SList

import Network.Wai
import qualified Network.HTTP.Types as HTTP

import qualified Data.ByteString as S
import Data.Maybe
import Data.Proxy

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
-- 
query :: (Query a, Strategy w, Monad m)
      => S.ByteString
      -> Proxy (w a)
      -> ApiaryT (SNext w as a) m b
      -> ApiaryT as m b
query k p = function $ \l r -> readStrategy k p (queryString r) l

--------------------------------------------------------------------------------

class Strategy (w :: * -> *) where
  type SNext w (as :: [*]) a  :: [*]
  readStrategy :: Query a => S.ByteString -> Proxy (w a)
            -> HTTP.Query -> SList as -> Maybe (SList (SNext w as a))

getQuery :: Query a => Proxy (w a) -> S.ByteString -> HTTP.Query -> [Maybe a]
getQuery _ k = map readQuery . map snd . filter ((k ==) . fst)

-- | get first matched key( [1,) params to Type.). since 0.5.0.0.
data Option a
instance Strategy Option where
    type SNext Option as a = Snoc as (Maybe a)
    readStrategy k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Just $ sSnoc l (Nothing `asMaybe` p)
           else case catMaybes rs of
               []  -> Just $ sSnoc l (Nothing `asMaybe` p)
               a:_ -> Just $ sSnoc l (Just a)
      where
        asMaybe :: Maybe a -> Proxy (w a) -> Maybe a
        asMaybe a _ = asProxyTypeOf a Proxy

-- | get first matched key ( [0,) params to Maybe Type.) since 0.5.0.0.
data First a
instance Strategy First where
    type SNext First as a = Snoc as a
    readStrategy k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               a:_ -> Just $ sSnoc l a

-- | get key ( [1] param to Type.) since 0.5.0.0.
data One a
instance Strategy One where
    type SNext One as a = Snoc as a
    readStrategy k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [a] -> Just $ sSnoc l a
               _   -> Nothing

-- | get parameters ( [0,) params to [Type] ) since 0.5.0.0.
data Many a
instance Strategy Many where
    type SNext Many as a = Snoc as [a]
    readStrategy k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else Just $ sSnoc l (catMaybes rs)

-- | get parameters ( [1,) params to [Type] ) since 0.5.0.0.
data Some a
instance Strategy Some where
    type SNext Some as a = Snoc as [a]
    readStrategy k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               as -> Just $ sSnoc l as

-- | type check ( [0,) params to No argument ) since 0.5.0.0.
data Check a
instance Strategy Check where
    type SNext Check as a = as
    readStrategy k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else case  catMaybes rs of
               [] -> Nothing
               _  -> Just l
