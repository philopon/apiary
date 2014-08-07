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
    hash POST            = 0
    hash HEAD            = 0
    hash PUT             = 0
    hash DELETE          = 0
    hash TRACE           = 0
    hash CONNECT         = 0
    hash OPTIONS         = 0
    hash PATCH           = 0
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


