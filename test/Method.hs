{-# LANGUAGE OverloadedStrings #-}
module Method where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Apiary.Method
import qualified Data.ByteString as S

stdMethods :: [S.ByteString]
stdMethods =
    [ "GET"
    , "POST"
    , "HEAD"
    , "PUT"
    , "DELETE"
    , "TRACE"
    , "CONNECT"
    , "OPTIONS"
    , "PATCH"
    ]

methodTests :: Test
methodTests = testGroup "Method"
    [ testCase "renderMethod . parseMethod == id" $
        mapM_ (\s -> 
            let s' = (renderMethod . parseMethod) s
            in assertBool (show s ++ " /= " ++ show s') $ s == s'
        ) ("YAMADA" : "neko" : "POSTa" : "DELET" : "CONNECt" : stdMethods)

    , testCase "parseMethod" $ do
        let assertMethod s t = assertBool (show s) $ parseMethod s == t
            assertNS s = assertMethod s (NonStandard s)
        assertMethod "GET"     GET
        assertMethod "POST"    POST
        assertMethod "HEAD"    HEAD
        assertMethod "PUT"     PUT
        assertMethod "DELETE"  DELETE
        assertMethod "TRACE"   TRACE
        assertMethod "CONNECT" CONNECT
        assertMethod "OPTIONS" OPTIONS
        assertMethod "PATCH"   PATCH
        assertNS "YAMADA"
        assertNS "neko"
        assertNS "POSTa"
        assertNS "DELET"
        assertNS "CONNECt"
    ]
