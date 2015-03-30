#!/usr/bin/env runghc

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
    c <- L.getContents
    let (pre, suf) = fmap L.tail $ L.break (== 0) c
    print (L.length pre)
    L.putStr pre
    L.putStr suf
