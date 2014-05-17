{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Apiary.Param (
    -- * for path capture
    Path(..)
    -- * for query capture
    , QueryElem(..)
    , Query(..)
    , First, One, Option, Many, Some, Check
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.HTTP.Types as HTTP
import Data.Text.Encoding.Error
import Text.Read
import Data.Int
import Data.Word
import Data.Maybe
import Data.Proxy
import Data.Apiary.SList

class Path a where
  readPath :: T.Text -> Maybe a

instance Path Char where
    readPath    s | T.null s  = Nothing
                  | otherwise = Just $ T.head s

instance Path Int     where readPath    = readMaybe . T.unpack
instance Path Int8    where readPath    = readMaybe . T.unpack
instance Path Int16   where readPath    = readMaybe . T.unpack
instance Path Int32   where readPath    = readMaybe . T.unpack
instance Path Int64   where readPath    = readMaybe . T.unpack
instance Path Integer where readPath    = readMaybe . T.unpack

instance Path Word   where readPath    = readMaybe . T.unpack
instance Path Word8  where readPath    = readMaybe . T.unpack
instance Path Word16 where readPath    = readMaybe . T.unpack
instance Path Word32 where readPath    = readMaybe . T.unpack
instance Path Word64 where readPath    = readMaybe . T.unpack

instance Path Double  where readPath    = readMaybe . T.unpack
instance Path Float   where readPath    = readMaybe . T.unpack

instance Path  T.Text      where readPath    = Just
instance Path TL.Text      where readPath    = Just . TL.fromStrict
instance Path S.ByteString where readPath    = Just . T.encodeUtf8
instance Path L.ByteString where readPath    = Just . TL.encodeUtf8 . TL.fromStrict
instance Path String       where readPath    = Just . T.unpack

--------------------------------------------------------------------------------

class QueryElem a where
    readQueryElem :: Maybe S.ByteString -> Maybe a

instance QueryElem Int     where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Int8    where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Int16   where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Int32   where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Int64   where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Integer where readQueryElem = maybe Nothing (readMaybe . S.unpack)

instance QueryElem Word   where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Word8  where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Word16 where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Word32 where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Word64 where readQueryElem = maybe Nothing (readMaybe . S.unpack)

instance QueryElem Double where readQueryElem = maybe Nothing (readMaybe . S.unpack)
instance QueryElem Float  where readQueryElem = maybe Nothing (readMaybe . S.unpack)

instance QueryElem T.Text       where readQueryElem = fmap $ T.decodeUtf8With lenientDecode
instance QueryElem TL.Text      where readQueryElem = fmap (TL.decodeUtf8With lenientDecode . L.fromStrict)
instance QueryElem S.ByteString where readQueryElem = id
instance QueryElem L.ByteString where readQueryElem = fmap L.fromStrict
instance QueryElem String       where readQueryElem = fmap S.unpack

-- | allow no parameter. but check parameter type.
instance QueryElem a => QueryElem (Maybe a) where
    readQueryElem (Just a) = Just `fmap` readQueryElem (Just a)
    readQueryElem Nothing  = Just Nothing

-- | always success. for exists check.
instance QueryElem () where
    readQueryElem _ = Just ()

--------------------------------------------------------------------------------

class Query (w :: * -> *) where
  type QSnoc w (as :: [*]) a  :: [*]
  readQuery :: QueryElem a => S.ByteString -> Proxy (w a)
            -> HTTP.Query -> SList as -> Maybe (SList (QSnoc w as a))

getQuery :: QueryElem a => Proxy (w a) -> S.ByteString -> HTTP.Query -> [Maybe a]
getQuery _ k = map readQueryElem . map snd . filter ((k ==) . fst)

asMaybe :: Maybe a -> Proxy (w a) -> Maybe a
asMaybe a _ = asProxyTypeOf a Proxy

-- | get first matched key( [1,) params to Type.). since 0.5.0.0.
data Option a
instance Query Option where
    type QSnoc Option as a = Snoc as (Maybe a)
    readQuery k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Just $ sSnoc l (Nothing `asMaybe` p)
           else case catMaybes rs of
               []  -> Just $ sSnoc l (Nothing `asMaybe` p)
               a:_ -> Just $ sSnoc l (Just a)

-- | get first matched key ( [0,) params to Maybe Type.) since 0.5.0.0.
data First a
instance Query First where
    type QSnoc First as a = Snoc as a
    readQuery k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               a:_ -> Just $ sSnoc l a

-- | get key ( [1] param to Type.) since 0.5.0.0.
data One a
instance Query One where
    type QSnoc One as a = Snoc as a
    readQuery k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [a] -> Just $ sSnoc l a
               _   -> Nothing

-- | get parameters ( [0,) params to [Type] ) since 0.5.0.0.
data Many a

instance Query Many where
    type QSnoc Many as a = Snoc as [a]
    readQuery k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else Just $ sSnoc l (catMaybes rs)

-- | get parameters ( [1,) params to [Type] ) since 0.5.0.0.
data Some a

instance Query Some where
    type QSnoc Some as a = Snoc as [a]
    readQuery k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               as -> Just $ sSnoc l as

-- | type check ( [0,) params to No argument ) since 0.5.0.0.
data Check a

instance Query Check where
    type QSnoc Check as a = as
    readQuery k p q l =
        let rs = getQuery p k q
        in if any isNothing rs
           then Nothing
           else case  catMaybes rs of
               [] -> Nothing
               _  -> Just l
