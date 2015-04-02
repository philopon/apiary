#!/usr/bin/env runghc

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    c <- L.getContents
    let (pre, suf) = fmap L.tail $ L.break (== '\0') c
    L.putStr . runPut $ do
        put pre
        putLazyByteString suf
