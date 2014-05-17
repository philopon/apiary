{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Apiary.Param where

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


