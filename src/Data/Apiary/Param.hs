{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Apiary.Param where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Read
import Data.Int
import Data.Word

class Param a where
  readParam :: T.Text -> Maybe a

instance Param Char where
    readParam s | T.null s  = Nothing
                | otherwise = Just $ T.head s

instance Param Int     where readParam = readMaybe . T.unpack
instance Param Int8    where readParam = readMaybe . T.unpack
instance Param Int16   where readParam = readMaybe . T.unpack
instance Param Int32   where readParam = readMaybe . T.unpack
instance Param Int64   where readParam = readMaybe . T.unpack
instance Param Integer where readParam = readMaybe . T.unpack

instance Param Word   where readParam = readMaybe . T.unpack
instance Param Word8  where readParam = readMaybe . T.unpack
instance Param Word16 where readParam = readMaybe . T.unpack
instance Param Word32 where readParam = readMaybe . T.unpack
instance Param Word64 where readParam = readMaybe . T.unpack

instance Param Double  where readParam = readMaybe . T.unpack
instance Param Float   where readParam = readMaybe . T.unpack

instance Param T.Text where
    readParam = Just

instance Param TL.Text where
    readParam = Just . TL.fromStrict

instance Param String where
    readParam = Just . T.unpack

