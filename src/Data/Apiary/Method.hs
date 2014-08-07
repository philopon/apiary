{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Apiary.Method where

import Data.String
import qualified Data.ByteString.Char8 as S
import Data.Hashable

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | OPTIONS
    | PATCH
    | NonStandard S.ByteString
    deriving (Eq, Ord, Read, Show)

instance Hashable Method where
    hash GET             = 0
    hash POST            = 1
    hash HEAD            = 2
    hash PUT             = 3
    hash DELETE          = 4
    hash TRACE           = 5
    hash CONNECT         = 6
    hash OPTIONS         = 7
    hash PATCH           = 8
    hash (NonStandard s) = hash s
    hashWithSalt salt x = salt `hashWithSalt` hash x

renderMethod :: Method -> S.ByteString
renderMethod = \case
    GET           -> "GET"
    POST          -> "POST"
    HEAD          -> "HEAD"
    PUT           -> "PUT"
    DELETE        -> "DELETE"
    TRACE         -> "TRACE"
    CONNECT       -> "CONNECT"
    OPTIONS       -> "OPTIONS"
    PATCH         -> "PATCH"
    NonStandard a -> a
{-# INLINE renderMethod #-}

instance IsString Method where
    fromString = NonStandard . S.pack
