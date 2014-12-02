{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Data.Apiary.Method where

import Data.Hashable(Hashable(..))
import Data.String(IsString(..))
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe as U

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

dispatchMethod :: a -> a -> a -> a -> a -> a -> a -> a -> a -> (S.ByteString -> a) -> S.ByteString -> a
dispatchMethod get post head_ put delete trace connect options patch ns s
    | S.length s < 2 = ns s
    | otherwise = case U.unsafeHead s of
        71 -> if S.length s == 3 && cc 1 69 && cc 2 84 then get else ns s
        80 -> case U.unsafeIndex s 1 of
            79 -> if S.length s == 4 && cc 2 83 && cc 3 84 then post  else ns s
            85 -> if S.length s == 3 && cc 2 84            then put   else ns s
            65 -> if S.length s == 5 && cc 2 84 && cc 3 67 && cc 4 72 then patch else ns s
            _  -> ns s
        72 -> if S.length s == 4 && cc 1 69 && cc 2 65 && cc 3 68 then head_ else ns s
        68 -> if S.length s == 6 && cc 1 69 && cc 2 76 && cc 3 69 && cc 4 84 && cc 5 69 then delete else ns s
        84 -> if S.length s == 5 && cc 1 82 && cc 2 65 && cc 3 67 && cc 4 69 then trace else ns s
        67 -> if S.length s == 7 && cc 1 79 && cc 2 78 && cc 3 78 && cc 4 69 && cc 5 67 && cc 6 84 then connect else ns s
        79 -> if S.length s == 7 && cc 1 80 && cc 2 84 && cc 3 73 && cc 4 79 && cc 5 78 && cc 6 83 then options else ns s
        _  -> ns s
  where
    cc i c = U.unsafeIndex s i == c

parseMethod :: S.ByteString -> Method
parseMethod = dispatchMethod GET POST HEAD PUT DELETE TRACE CONNECT OPTIONS PATCH NonStandard

instance IsString Method where
    fromString = parseMethod . S.pack
