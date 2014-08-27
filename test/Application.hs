{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Application where

import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Monad

import Web.Apiary
import Network.Wai
import Network.Wai.Test
import qualified Network.HTTP.Types as HTTP

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S

testReq :: String -> (Request -> IO ()) -> Test
testReq str f = 
    let (meth, other) = break (== ' ') str
        (p,  version) = break (== ' ') (tail other)
    in testCase str $ f (setPath (setVersion version $ (defaultRequest { requestMethod = S.pack meth })) (S.pack p))
  where
    setVersion [] r = r
    setVersion v r | v == " HTTP/1.1" = r { Network.Wai.httpVersion = HTTP.http11 }
                   | v == " HTTP/1.0" = r { Network.Wai.httpVersion = HTTP.http10 }
                   | v == " HTTP/0.9" = r { Network.Wai.httpVersion = HTTP.http09 }
                   | otherwise        = error "unknown HTTP version"

--------------------------------------------------------------------------------

assertPlain200 :: L.ByteString -> Application -> Request -> IO ()
assertPlain200 body app req = flip runSession app $ do
    res <- request req
    assertBody body res
    assertStatus 200 res
    assertContentType "text/plain" res

assertHtml200 :: L.ByteString -> Application -> Request -> IO ()
assertHtml200 body app req = flip runSession app $ do
    res <- request req
    assertBody body res
    assertStatus 200 res
    assertContentType "text/html" res

assert404 :: Application -> Request -> IO ()
assert404 app req = flip runSession app $ do
    res <- request req
    assertBody "404 Page Notfound.\n" res
    assertStatus 404 res
    assertContentType "text/plain" res

--------------------------------------------------------------------------------

helloWorldApp :: Application
helloWorldApp = runApiary def $ action $ do
    contentType "text/plain"
    bytes "hello"

helloWorldAllTest :: Test
helloWorldAllTest = testGroup "helloWorld" $ map ($ helloWorldApp)
    [ testReq "GET /"    . assertPlain200 "hello"
    , testReq "GET /foo" . assertPlain200 "hello"
    , testReq "POST /"   . assertPlain200 "hello"
    ]

--------------------------------------------------------------------------------

methodFilterApp :: Application
methodFilterApp = runApiary def $ do
    method "GET" . action $ contentType "text/plain" >> bytes "GET"
    method POST  . action $ contentType "text/plain" >> bytes "POST"

methodFilterTest :: Test
methodFilterTest = testGroup "methodFilter" $ map ($methodFilterApp)
    [ testReq "GET /"    . assertPlain200 "GET"
    , testReq "POST /"   . assertPlain200 "POST"
    , testReq "GET /foo" . assertPlain200 "GET"
    , testReq "DELETE /" . assert404
    ]

--------------------------------------------------------------------------------

httpVersionApp :: Application
httpVersionApp = runApiary def $ do
    http09 . action $ contentType "text/plain" >> bytes "09"
    http10 . action $ contentType "text/plain" >> bytes "10"
    http11 . action $ contentType "text/plain" >> bytes "11"

httpVersionTest :: Test
httpVersionTest = testGroup "httpVersionFilter" $ map ($ httpVersionApp)
    [ testReq "GET / HTTP/0.9" . assertPlain200 "09"
    , testReq "GET / HTTP/1.0" . assertPlain200 "10"
    , testReq "GET / HTTP/1.1" . assertPlain200 "11"
    ]

--------------------------------------------------------------------------------

rootFilterApp :: Application
rootFilterApp = runApiary def .  root . action $ do
    contentType "text/html"
    bytes "root"

rootFilterTest :: Test
rootFilterTest = testGroup "rootFilter" $ map ($ rootFilterApp)
    [ testReq "GET /"           . assertHtml200 "root"
    , testReq "POST /"          . assertHtml200 "root"
    , testReq "GET /neko"       . assert404
    , testReq "GET /index.html" . assertHtml200 "root"
    ]

--------------------------------------------------------------------------------
anyFilterApp :: Application
anyFilterApp = runApiary def $ [capture|/test|] . anyPath . action $ do
    contentType "text/plain"
    bytes "hello"

anyFilterTest :: Test
anyFilterTest = testGroup "anyPath" $ map ($ anyFilterApp)
    [ testReq "GET /"          . assert404
    , testReq "GET /test"      . assertPlain200 "hello"
    , testReq "POST /test/foo" . assertPlain200 "hello"
    ]

--------------------------------------------------------------------------------

captureApp :: Application
captureApp = runApiary def $ do
    [capture|/foo|]  . action $ contentType "text/plain" >> bytes "foo"
    [capture|/:Int|] . method GET . action $ \i -> contentType "text/plain" >> bytes "Int " >> showing i
    [capture|/:Double|] . action $ \i -> contentType "text/plain" >> bytes "Double " >> showing i
    [capture|/bar/:L.ByteString/:Int|] . action $ \s i -> contentType "text/plain" >> lazyBytes s >> char ' ' >> showing i
    [capture|/:L.ByteString|] . action $ \s -> contentType "text/plain" >> bytes "fall " >> lazyBytes s

captureTest :: Test
captureTest = testGroup "capture" $ map ($ captureApp)
    [ testReq "GET /foo"  . assertPlain200 "foo"
    , testReq "GET /12"   . assertPlain200 "Int 12"
    , testReq "GET /12.4" . assertPlain200 "Double 12.4"
    , testReq "POST /12"  . assertPlain200 "Double 12.0"
    , testReq "GET /bar"  . assertPlain200 "fall bar"
    , testReq "GET /baz"  . assertPlain200 "fall baz"
    , testReq "GET /bar/nyan/12"       . assertPlain200 "nyan 12"
    , testReq "GET /bar/nyan/12/other" . assert404
    ]

--------------------------------------------------------------------------------

queryApp f g h = runApiary def $ do
    _ <- (f "foo" pInt)        . action $ \i -> contentType "text/plain" >> bytes "foo Int " >> showing i
    _ <- (g "foo" pString)     . action $ \i -> contentType "text/plain" >> bytes "foo String " >> showing i
    (h "foo" (pMaybe pString)) . action $ \i -> contentType "text/plain" >> bytes "foo Maybe String " >> showing i

queryOptionalApp :: Application
queryOptionalApp = runApiary def $ do
    ("foo" =?!: (5 :: Int))                   . action $ \i -> contentType "text/plain" >> bytes "foo Int " >> showing i
    ("foo" =?!: ("bar" :: String))            . action $ \i -> contentType "text/plain" >> bytes "foo String " >> showing i
    ("foo" =?!: (Just "baz" :: Maybe String)) . action $ \i -> contentType "text/plain" >> bytes "foo Maybe String " >> showing i

queryCheckApp :: Application
queryCheckApp = runApiary def $ do
    ("foo" ?: pInt)           . action $ contentType "text/plain" >> bytes "foo Int"
    ("foo" ?: pString)        . action $ contentType "text/plain" >> bytes "foo String"
    ("foo" ?: pMaybe pString) . action $ contentType "text/plain" >> bytes "foo Maybe String"

queryFirstTest :: Test
queryFirstTest = testGroup "First" $ map ($ queryApp (=:) (=:) (=:))
    [ testReq "GET /" . assert404
    , testReq "GET /?foo" . assertPlain200 "foo Maybe String Nothing"
    , testReq "GET /?foo&foo=3" . assertPlain200 "foo Maybe String Nothing"
    , testReq "GET /?foo=12" . assertPlain200 "foo Int 12"
    , testReq "GET /?foo=a" . assertPlain200 "foo String \"a\""
    , testReq "GET /?foo=12&foo=23" . assertPlain200 "foo Int 12"
    , testReq "GET /?foo=12&foo=b" . assertPlain200 "foo Int 12"
    ]

queryOneTest :: Test
queryOneTest = testGroup "One" $ map ($ queryApp (=!:) (=!:) (=!:))
    [ testReq "GET /" . assert404
    , testReq "GET /?foo" . assertPlain200 "foo Maybe String Nothing"
    , testReq "GET /?foo&foo=3" . assert404
    , testReq "GET /?foo=12" . assertPlain200 "foo Int 12"
    , testReq "GET /?foo=a" . assertPlain200 "foo String \"a\""
    , testReq "GET /?foo=12&foo=23" . assert404
    , testReq "GET /?foo=12&foo=b" . assert404
    ]

queryOptionTest :: Test
queryOptionTest = testGroup "Option" $ map ($ queryApp (=?:) (=?:) (=?:))
    [ testReq "GET /" . assertPlain200 "foo Int Nothing"
    , testReq "GET /?foo" . assertPlain200 "foo Maybe String Just Nothing"
    , testReq "GET /?foo&foo=3" . assertPlain200 "foo Maybe String Just Nothing"
    , testReq "GET /?foo=12" . assertPlain200 "foo Int Just 12"
    , testReq "GET /?foo=a" . assertPlain200 "foo String Just \"a\""
    , testReq "GET /?foo=12&foo=23" . assertPlain200 "foo Int Just 12"
    , testReq "GET /?foo=12&foo=b" . assertPlain200 "foo String Just \"12\""
    ]

queryOptionalTest :: Test
queryOptionalTest = testGroup "Optional" $ map ($ queryOptionalApp)
    [ testReq "GET /" . assertPlain200 "foo Int 5"
    , testReq "GET /?foo" . assertPlain200 "foo Maybe String Nothing"
    , testReq "GET /?foo&foo=3" . assertPlain200 "foo Maybe String Nothing"
    , testReq "GET /?foo=12" . assertPlain200 "foo Int 12"
    , testReq "GET /?foo=a" . assertPlain200 "foo String \"a\""
    , testReq "GET /?foo=12&foo=23" . assertPlain200 "foo Int 12"
    , testReq "GET /?foo=12&foo=b" . assertPlain200 "foo String \"12\""
    ]

queryCheckTest :: Test
queryCheckTest = testGroup "Check" $ map ($ queryCheckApp)
    [ testReq "GET /" . assert404
    , testReq "GET /?foo" . assertPlain200 "foo Maybe String"
    , testReq "GET /?foo&foo=3" . assertPlain200 "foo Maybe String"
    , testReq "GET /?foo=12" . assertPlain200 "foo Int"
    , testReq "GET /?foo=a" . assertPlain200 "foo String"
    , testReq "GET /?foo=12&foo=23" . assertPlain200 "foo Int"
    , testReq "GET /?foo=12&foo=b" . assertPlain200 "foo String"
    ]

queryManyTest :: Test
queryManyTest = testGroup "Many" $ map ($ queryApp (=*:) (=*:) (=*:))
    [ testReq "GET /" . assertPlain200 "foo Int []"
    , testReq "GET /?foo" . assertPlain200 "foo Maybe String [Nothing]"
    , testReq "GET /?foo&foo=3" . assertPlain200 "foo Maybe String [Nothing,Just \"3\"]"
    , testReq "GET /?foo=12" . assertPlain200 "foo Int [12]"
    , testReq "GET /?foo=a" . assertPlain200 "foo String [\"a\"]"
    , testReq "GET /?foo=12&foo=23" . assertPlain200 "foo Int [12,23]"
    , testReq "GET /?foo=12&foo=b" . assertPlain200 "foo String [\"12\",\"b\"]"
    ]

querySomeTest :: Test
querySomeTest = testGroup "Some" $ map ($ queryApp (=+:) (=+:) (=+:))
    [ testReq "GET /" . assert404
    , testReq "GET /?foo" . assertPlain200 "foo Maybe String [Nothing]"
    , testReq "GET /?foo&foo=3" . assertPlain200 "foo Maybe String [Nothing,Just \"3\"]"
    , testReq "GET /?foo=12" . assertPlain200 "foo Int [12]"
    , testReq "GET /?foo=a" . assertPlain200 "foo String [\"a\"]"
    , testReq "GET /?foo=12&foo=23" . assertPlain200 "foo Int [12,23]"
    , testReq "GET /?foo=12&foo=b" . assertPlain200 "foo String [\"12\",\"b\"]"
    ]


queryTest :: Test
queryTest = testGroup "query"
    [ queryFirstTest
    , queryOneTest
    , queryOptionTest
    , queryOptionalTest
    , queryCheckTest
    , queryManyTest
    , querySomeTest
    ]

--------------------------------------------------------------------------------
stopApp :: Application
stopApp = runApiary def $ do
    [capture|/a/:Int|] . action $ \i -> do
        contentType "text/plain"
        when (i == 1) $ bytes "one\n"
        if i `mod` 2 == 0 then bytes "even\n" else bytes "odd\n"
        when (i == 2) stop
        bytes "after stop"

stopTest :: Test
stopTest = testGroup "stop" $ map ($ stopApp)
    [ testReq "GET /a/0" . assertPlain200 "even\nafter stop"
    , testReq "GET /a/1" . assertPlain200 "one\nodd\nafter stop"
    , testReq "GET /a/2" . assertPlain200 "even\n"
    , testReq "GET /a/3" . assertPlain200 "odd\nafter stop"
    ]

--------------------------------------------------------------------------------

multipleFilter1App :: Application
multipleFilter1App = runApiary def $ do
    root $ do
        method GET  . action $ contentType "text/plain" >> bytes "GET /"
        method POST . action $ contentType "text/html"  >> bytes "POST /"

    method DELETE . action   $ contentType "text/plain" >> bytes "DELETE ANY"

multipleFilter1Test :: Test
multipleFilter1Test = testGroup "multiple test1: root, method"
    [ testReq "GET /index.html" $ assertPlain200 "GET /"      multipleFilter1App
    , testReq "POST /"          $ assertHtml200 "POST /"      multipleFilter1App
    , testReq "DELETE /"        $ assertPlain200 "DELETE ANY" multipleFilter1App 
    , testReq "PUT /"           $ assert404 multipleFilter1App
    ]

--------------------------------------------------------------------------------

applicationTests :: Test
applicationTests = testGroup "Application"
    [ helloWorldAllTest
    , methodFilterTest
    , httpVersionTest
    , rootFilterTest
    , anyFilterTest
    , captureTest
    , queryTest
    , stopTest
    , multipleFilter1Test
    ]

