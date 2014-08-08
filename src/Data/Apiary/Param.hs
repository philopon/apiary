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
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Data.Apiary.Param where

import Network.Wai

import Data.Int
import Data.Word
import Data.Proxy
import Data.Apiary.Proxy

import Data.String(IsString)
import qualified Data.Text.Read as TR
import Data.Text.Encoding.Error
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Data.ByteString.Lex.Integral as SL
import qualified Data.ByteString.Lex.Double   as SL

jsToBool :: (IsString a, Eq a) => a -> Bool
jsToBool = flip notElem jsFalse
  where
    jsFalse = ["false", "0", "-0", "", "null", "undefined", "NaN"]

readPathAs :: Path a => proxy a -> T.Text -> Maybe a
readPathAs _ t = readPath t
{-# INLINE readPathAs #-}

data Text deriving Typeable

type Param = (S.ByteString, S.ByteString)

data File = File
    { fileParameter   :: S.ByteString
    , fileName        :: S.ByteString
    , fileContentType :: S.ByteString
    , fileContent     :: L.ByteString
    } deriving (Show, Eq, Typeable)

data QueryRep
    = Strict   TypeRep
    | Nullable TypeRep
    | Check
    deriving (Show, Eq)

class Typeable a => Path a where
    readPath :: T.Text  -> Maybe a
    pathRep  :: proxy a -> TypeRep
    pathRep = typeRep

instance Path Char where
    readPath s
        | T.null s  = Nothing
        | otherwise = Just $ T.head s

readText :: TR.Reader a -> T.Text -> (Maybe a)
readText p s = case p s of
    Right (a, t) -> if T.null t then Just a else Nothing
    Left   _     -> Nothing

readTextInt :: Integral i => T.Text -> Maybe i
readTextInt = readText (TR.signed TR.decimal)

readTextWord :: Integral i => T.Text -> Maybe i
readTextWord = readText TR.decimal

readTextDouble :: T.Text -> Maybe Double
readTextDouble = readText TR.double

-- | javascript boolean.
-- when \"false\", \"0\", \"-0\", \"\", \"null\", \"undefined\", \"NaN\" then False, else True. since 0.6.0.0.
instance Path Bool    where readPath = Just . jsToBool

instance Path Int     where readPath = readTextInt
instance Path Int8    where readPath = readTextInt
instance Path Int16   where readPath = readTextInt
instance Path Int32   where readPath = readTextInt
instance Path Int64   where readPath = readTextInt
instance Path Integer where readPath = readTextInt

instance Path Word    where readPath = readTextWord
instance Path Word8   where readPath = readTextWord
instance Path Word16  where readPath = readTextWord
instance Path Word32  where readPath = readTextWord
instance Path Word64  where readPath = readTextWord

instance Path Double  where readPath = readTextDouble
instance Path Float   where readPath = fmap realToFrac . readTextDouble

instance Path  T.Text      where readPath = Just;                 pathRep _ = typeRep (Proxy :: Proxy Text)
instance Path TL.Text      where readPath = Just . TL.fromStrict; pathRep _ = typeRep (Proxy :: Proxy Text)
instance Path S.ByteString where readPath = Just . T.encodeUtf8;  pathRep _ = typeRep (Proxy :: Proxy Text)
instance Path L.ByteString where readPath = Just . TL.encodeUtf8 . TL.fromStrict; pathRep _ = typeRep (Proxy :: Proxy Text)
instance Path String       where readPath = Just . T.unpack;      pathRep _ = typeRep (Proxy :: Proxy Text)

--------------------------------------------------------------------------------

class Typeable a => Query a where
    readQuery :: Maybe S.ByteString -> Maybe a
    queryRep  :: proxy a            -> QueryRep
    queryRep = Strict . qTypeRep
    qTypeRep  :: proxy a            -> TypeRep
    qTypeRep = typeRep

readBS :: (S.ByteString -> Maybe (a, S.ByteString))
       -> S.ByteString -> Maybe a
readBS p b = case p b of
    Just (i, s) -> if S.null s then Just i else Nothing
    _           -> Nothing

readBSInt :: Integral a => S.ByteString -> Maybe a
readBSInt = readBS (SL.readSigned SL.readDecimal)

readBSWord :: Integral a => S.ByteString -> Maybe a
readBSWord = readBS SL.readDecimal

readBSDouble :: S.ByteString -> Maybe Double
readBSDouble = readBS SL.readDouble

-- | javascript boolean.
-- when \"false\", \"0\", \"-0\", \"\", \"null\", \"undefined\", \"NaN\" then False, else True. since 0.6.0.0.
instance Query Bool    where readQuery = fmap jsToBool

instance Query Int     where readQuery = maybe Nothing readBSInt
instance Query Int8    where readQuery = maybe Nothing readBSInt
instance Query Int16   where readQuery = maybe Nothing readBSInt
instance Query Int32   where readQuery = maybe Nothing readBSInt
instance Query Int64   where readQuery = maybe Nothing readBSInt
instance Query Integer where readQuery = maybe Nothing readBSInt

instance Query Word    where readQuery = maybe Nothing readBSWord
instance Query Word8   where readQuery = maybe Nothing readBSWord
instance Query Word16  where readQuery = maybe Nothing readBSWord
instance Query Word32  where readQuery = maybe Nothing readBSWord
instance Query Word64  where readQuery = maybe Nothing readBSWord

instance Query Double  where readQuery = maybe Nothing readBSDouble
instance Query Float   where readQuery = maybe Nothing (fmap realToFrac . readBSDouble)

instance Query T.Text  where
    readQuery  = fmap $ T.decodeUtf8With lenientDecode
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

instance Query TL.Text where
    readQuery  = fmap (TL.decodeUtf8With lenientDecode . L.fromStrict)
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

instance Query S.ByteString where 
    readQuery  = id
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

instance Query L.ByteString where 
    readQuery  = fmap L.fromStrict
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

instance Query String       where
    readQuery  = fmap S.unpack
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

-- | allow no parameter. but check parameter type.
instance Query a => Query (Maybe a) where
    readQuery (Just a) = Just `fmap` readQuery (Just a)
    readQuery Nothing  = Just Nothing
    queryRep _         = Nullable $ qTypeRep (Proxy :: Proxy a)

-- | always success. for check existence.
instance Query () where
    readQuery _ = Just ()
    queryRep  _ = Check
    qTypeRep  _ = typeOf ()

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

pMaybe :: proxy a -> Proxy (Maybe a)
pMaybe _ = Proxy

pFile :: Proxy File
pFile = Proxy

class ReqParam a where
    reqParams   :: proxy a -> Request -> [Param] -> [File] -> [(S.ByteString, Maybe a)]
    reqParamRep :: proxy a -> QueryRep

instance ReqParam File where
    reqParams _ _ _ = map (\f -> (fileParameter f, Just f))
    reqParamRep   _ = Strict $ typeRep pFile

instance Query a => ReqParam a where
    reqParams _ r p _ = map (\(k,v) -> (k, readQuery v)) (queryString r) ++
        map (\(k,v) -> (k, readQuery $ Just v)) p
    reqParamRep = queryRep
