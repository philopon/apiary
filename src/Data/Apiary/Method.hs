{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Apiary.Method where

import Data.String
import qualified Data.ByteString.Char8 as S

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


