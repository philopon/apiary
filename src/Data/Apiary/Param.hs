{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}

module Data.Apiary.Param where

import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Encoding.Error
import Text.Read
import Data.Int
import Data.Word
import Data.Proxy
import Data.String(IsString)
import Data.Maybe

import Network.Wai
import Network.Wai.Parse

jsToBool :: (IsString a, Eq a) => a -> Bool
jsToBool = flip notElem jsFalse
    where
      jsFalse = ["false", "0", "-0", "", "null", "undefined", "NaN"]


class Path a where
  readPath :: T.Text -> Maybe a

instance Path Char where
    readPath    s | T.null s  = Nothing
                  | otherwise = Just $ T.head s

-- | javascript boolean.
-- when \"false\", \"0\", \"-0\", \"\", \"null\", \"undefined\", \"NaN\" then False, else True. since 0.6.0.0.
instance Path Bool where readPath = Just . jsToBool

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

class Query a where
    readQuery :: Maybe S.ByteString -> Maybe a

-- | javascript boolean.
-- when \"false\", \"0\", \"-0\", \"\", \"null\", \"undefined\", \"NaN\" then False, else True. since 0.6.0.0.
instance Query Bool where readQuery = fmap jsToBool

instance Query Int     where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Int8    where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Int16   where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Int32   where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Int64   where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Integer where readQuery = maybe Nothing (readMaybe . S.unpack)

instance Query Word   where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Word8  where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Word16 where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Word32 where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Word64 where readQuery = maybe Nothing (readMaybe . S.unpack)

instance Query Double where readQuery = maybe Nothing (readMaybe . S.unpack)
instance Query Float  where readQuery = maybe Nothing (readMaybe . S.unpack)

instance Query T.Text       where readQuery = fmap $ T.decodeUtf8With lenientDecode
instance Query TL.Text      where readQuery = fmap (TL.decodeUtf8With lenientDecode . L.fromStrict)
instance Query S.ByteString where readQuery = id
instance Query L.ByteString where readQuery = fmap L.fromStrict
instance Query String       where readQuery = fmap S.unpack

-- | allow no parameter. but check parameter type.
instance Query a => Query (Maybe a) where
    readQuery (Just a) = Just `fmap` readQuery (Just a)
    readQuery Nothing  = Just Nothing

-- | always success. for exists check.
instance Query () where
    readQuery _ = Just ()

pBool :: Proxy Bool
pBool = Proxy

pInt :: Proxy Int
pInt = Proxy
pInt8 :: Proxy Int8
pInt8 = Proxy
pInt16 :: Proxy Int16
pInt16 = Proxy
pInt32 :: Proxy Int32
pInt32 = Proxy
pInt64 :: Proxy Int64
pInt64 = Proxy
pInteger :: Proxy Integer
pInteger = Proxy

pWord :: Proxy Word
pWord = Proxy
pWord8 :: Proxy Word8
pWord8 = Proxy
pWord16 :: Proxy Word16
pWord16 = Proxy
pWord32 :: Proxy Word32
pWord32 = Proxy
pWord64 :: Proxy Word64
pWord64 = Proxy

pDouble :: Proxy Double
pDouble = Proxy
pFloat :: Proxy Float
pFloat = Proxy

pText :: Proxy T.Text
pText = Proxy
pLazyText :: Proxy TL.Text
pLazyText = Proxy
pByteString :: Proxy S.ByteString
pByteString = Proxy
pLazyByteString :: Proxy L.ByteString
pLazyByteString = Proxy
pString :: Proxy String
pString = Proxy

pVoid :: Proxy ()
pVoid = Proxy

pMaybe :: Proxy a -> Proxy (Maybe a)
pMaybe _ = Proxy

pFile :: Proxy (File L.ByteString)
pFile = Proxy

class ReqParam a where
  reqParams :: Proxy a -> Request -> [Param] -> [File L.ByteString] -> [(S.ByteString, a)]

instance ReqParam (FileInfo L.ByteString) where
    reqParams _ _ _ f = f

instance Query a => ReqParam a where
    reqParams _ r p _ = mapMaybe (\(k,v) -> (k,) <$> readQuery v) (queryString r) ++
        mapMaybe (\(k,v) -> (k,) <$> readQuery (Just v)) p
