{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Apiary.Param where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as S
import Text.Read
import Data.Int
import Data.Word

class Param a where
  readPath  :: T.Text -> Maybe a
  readQuery :: (S.ByteString -> T.Text) -> S.ByteString -> Maybe a

instance Param Char where
    readPath    s | T.null s  = Nothing
                  | otherwise = Just $ T.head s
    readQuery _ s | S.null s = Nothing
                  | otherwise = Just $ S.head s

instance Param Int     where 
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Int8    where
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Int16   where
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Int32   where 
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Int64   where 
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Integer where
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Word   where
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Word8  where
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Word16 where 
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Word32 where
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Word64 where 
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Double  where
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param Float   where 
    readPath    = readMaybe . T.unpack
    readQuery _ = readMaybe . S.unpack

instance Param T.Text where
    readPath      = Just
    readQuery e s = Just $ e s

instance Param TL.Text where
    readPath      = Just . TL.fromStrict
    readQuery e s = Just . TL.fromStrict $ e s

instance Param String where
    readPath    = Just . T.unpack
    readQuery _ = Just . S.unpack

