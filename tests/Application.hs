{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds #-}

module Application(test) where

import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)

import Control.Monad(when)
import Control.Monad.Identity(Identity(..))

import Web.Apiary
import Network.Wai(Request)
import qualified Network.Wai.Test as WT
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S

testReq :: String -> (Request -> IO ()) -> Test
testReq str f = 
    let (meth, other) = break (== ' ') str
        (p,  version) = break (== ' ') (tail other)
    in testCase str $ f (WT.setPath (setVersion version $ (WT.defaultRequest { Wai.requestMethod = S.pack meth })) (S.pack p))
  where
    setVersion [] r = r
    setVersion v r | v == " HTTP/1.0" = r { Wai.httpVersion = HTTP.http10 }
                   | v == " HTTP/0.9" = r { Wai.httpVersion = HTTP.http09 }
                   | otherwise        = r { Wai.httpVersion = HTTP.http11 }
--------------------------------------------------------------------------------

assertRequest :: Int -> (Maybe S.ByteString) -> L.ByteString -> Application -> Request -> IO ()
assertRequest sc ct body app req = flip WT.runSession app $ do
    res <- WT.request req
    WT.assertBody body res
    WT.assertStatus sc res
    maybe (return ()) (flip WT.assertContentType res) ct

assertPlain200 :: L.ByteString -> Application -> Request -> IO ()
assertPlain200 = assertRequest 200 (Just "text/plain")

assertHtml200 :: L.ByteString -> Application -> Request -> IO ()
assertHtml200 = assertRequest 200 (Just "text/html")

assertJson200 :: L.ByteString -> Application -> Request -> IO ()
assertJson200 = assertRequest 200 (Just "application/json")

assert404 :: Application -> Request -> IO ()
assert404 = assertRequest 404 (Just "text/plain") "404 Page Notfound.\n"

--------------------------------------------------------------------------------
runApp :: ApiaryT '[] '[] IO Identity () -> Application
runApp = runIdentity . runApiary return def
--------------------------------------------------------------------------------

helloWorldApp :: Application
helloWorldApp = runApp $ action $ do
    contentType "text/plain"
    bytes "hello"

helloWorldAllTest :: Test
helloWorldAllTest = testGroup "helloWorld" $ map ($ helloWorldApp)
    [ testReq "GET /"    . assertPlain200 "hello"
    , testReq "GET /foo" . assert404
    , testReq "POST /"   . assertPlain200 "hello"
    ]

--------------------------------------------------------------------------------

methodFilterApp :: Application
methodFilterApp = runApp $ do
    method "GET" . action $ contentType "text/plain" >> bytes "GET"
    method POST  . action $ contentType "text/plain" >> bytes "POST"

methodFilterTest :: Test
methodFilterTest = testGroup "methodFilter" $ map ($methodFilterApp)
    [ testReq "GET /"    . assertPlain200 "GET"
    , testReq "POST /"   . assertPlain200 "POST"
    , testReq "GET /foo" . assert404
    , testReq "DELETE /" . assert404
    ]

--------------------------------------------------------------------------------

httpVersionApp :: Application
httpVersionApp = runApp $ do
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
rootFilterApp = runApp . root . action $ do
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

restFilterApp :: Application
restFilterApp = runApp $ do
    [capture|/test/**rest|] . action $ contentType "text/plain" >> param [key|rest|] >>= showing
    [capture|/test/neko|] . action $ contentType "text/plain" >> bytes "nyan"

restFilterTest :: Test
restFilterTest = testGroup "rest capture" $ map ($ restFilterApp)
    [ testReq "GET /" . assert404
    , testReq "GET /test" . assertPlain200 "[]"
    , testReq "GET /test/foo" . assertPlain200 "[\"foo\"]"
    , testReq "GET /test/foo/bar" . assertPlain200 "[\"foo\",\"bar\"]"
    , testReq "GET /test/neko" . assertPlain200 "nyan"
    ]

--------------------------------------------------------------------------------

captureApp :: Application
captureApp = runApp $ do
    [capture|/foo|]  . action $ contentType "text/plain" >> bytes "foo"
    [capture|/int::Int|] . method GET . action $ contentType "text/plain" >> bytes "Int " >> param [key|int|] >>= appendShowing
    [capture|/d::Double|] . action $ contentType "text/plain" >> bytes "Double " >> param [key|d|] >>= appendShowing
    [capture|/bar/s::L.ByteString/i::Int|] . action $ contentType "text/plain" >> param [key|s|] >>= lazyBytes >> appendChar ' ' >> param [key|i|] >>= appendShowing
    [capture|/s::L.ByteString|] . action $ contentType "text/plain" >> bytes "fall " >> param [key|s|] >>= appendLazyBytes

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

queryApp f g h = runApp $ do
    _ <- (f [key|foo|] pInt)    . action $ contentType "text/plain" >> bytes "foo Int " >> param [key|foo|] >>= appendShowing
    _ <- (g [key|foo|] pString) . action $ contentType "text/plain" >> bytes "foo String " >> param [key|foo|] >>= appendShowing
    (h [key|foo|] (pMaybe pString)) . action $ contentType "text/plain" >> bytes "foo Maybe String " >> param [key|foo|] >>= appendShowing

queryOptionalApp :: Application
queryOptionalApp = runApp $ do
    ([key|foo|] =?!: (5 :: Int))                   . action $ contentType "text/plain" >> bytes "foo Int " >> param [key|foo|] >>= appendShowing
    ([key|foo|] =?!: ("bar" :: String))            . action $ contentType "text/plain" >> bytes "foo String " >> param [key|foo|] >>= appendShowing
    ([key|foo|] =?!: (Just "baz" :: Maybe String)) . action $ contentType "text/plain" >> bytes "foo Maybe String " >> param [key|foo|] >>= appendShowing

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
    , testReq "GET /?foo=12&foo=b" . assertPlain200 "foo Int Just 12"
    ]

queryOptionalTest :: Test
queryOptionalTest = testGroup "Optional" $ map ($ queryOptionalApp)
    [ testReq "GET /" . assertPlain200 "foo Int 5"
    , testReq "GET /?foo" . assertPlain200 "foo Maybe String Nothing"
    , testReq "GET /?foo&foo=3" . assertPlain200 "foo Maybe String Nothing"
    , testReq "GET /?foo=12" . assertPlain200 "foo Int 12"
    , testReq "GET /?foo=a" . assertPlain200 "foo String \"a\""
    , testReq "GET /?foo=12&foo=23" . assertPlain200 "foo Int 12"
    , testReq "GET /?foo=12&foo=b" . assertPlain200 "foo Int 12"
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

switchQueryApp :: Application
switchQueryApp = runApp $ do
    switchQuery [key|foo|] . switchQuery [key|bar|] . action $ do
        contentType "text/plain"
        param [key|foo|] >>= showing
        param [key|bar|] >>= appendShowing

switchQueryTest :: Test
switchQueryTest = testGroup "switch" $ map ($ switchQueryApp)
    [ testReq "GET /"                    . assertPlain200 "FalseFalse"
    , testReq "GET /?foo"                . assertPlain200 "TrueFalse"
    , testReq "GET /?foo&bar"            . assertPlain200 "TrueTrue"
    , testReq "GET /?foo=true"           . assertPlain200 "TrueFalse"
    , testReq "GET /?foo=false"          . assertPlain200 "FalseFalse"
    , testReq "GET /?foo=false&bar=true" . assertPlain200 "FalseTrue"
    , testReq "GET /?foo&bar=true"       . assertPlain200 "TrueTrue"
    , testReq "GET /?foo=1&bar=0"        . assertPlain200 "TrueFalse"
    ]

queryTest :: Test
queryTest = testGroup "query"
    [ queryFirstTest
    , queryOneTest
    , queryOptionTest
    , queryOptionalTest
    , queryManyTest
    , querySomeTest
    , switchQueryTest
    ]

--------------------------------------------------------------------------------
stopApp :: Application
stopApp = runApp $ do
    [capture|/a/i::Int|] . action $ do
        i <- param [key|i|]
        contentType "text/plain"
        when (i == 1) $ bytes "one\n"
        if i `mod` 2 == 0 then appendBytes "even\n" else appendBytes "odd\n"
        when (i == 2) stop
        appendBytes "after stop"

stopTest :: Test
stopTest = testGroup "stop" $ map ($ stopApp)
    [ testReq "GET /a/0" . assertPlain200 "even\nafter stop"
    , testReq "GET /a/1" . assertPlain200 "one\nodd\nafter stop"
    , testReq "GET /a/2" . assertPlain200 "even\n"
    , testReq "GET /a/3" . assertPlain200 "odd\nafter stop"
    ]

--------------------------------------------------------------------------------

acceptApp :: Application
acceptApp = runApp $ [capture|/|] $ do
    accept "application/json" . action $ bytes "json"
    accept "text/html;prm=t"  . action $ bytes "html+prm"
    accept "text/html"        . action $ bytes "html"
    action                             $ bytes "other"

acceptTest :: Test
acceptTest = testGroup "accept" $ map ($ acceptApp)
    [ testReq "GET / application/json" . (\a r -> assertJson200 "json"     a $ addA "application/json" r)
    , testReq "GET / text/html"        . (\a r -> assertHtml200 "html"     a $ addA "text/html"     r)
    , testReq "GET / text/html;p=a"    . (\a r -> assertHtml200 "html"     a $ addA "text/html;p=a" r)
    , testReq "GET / text/html;prm=t"  . (\a r -> assertHtml200 "html+prm" a $ addA "text/html;prm=t" r)
    , testReq "GET / text/plain"       . (\a r -> assertRequest 200 Nothing "other" a $ addA "text/plain" r)
    , testReq "GET / text/*"           . (\a r -> assertHtml200 "html"     a $ addA "text/*" r)
    , testReq "GET / text/*;p=a"       . (\a r -> assertHtml200 "html"     a $ addA "text/*;p=a" r)
    , testReq "GET / */*"              . (\a r -> assertJson200 "json"     a $ addA "*/*" r)
    , testReq "GET /"                  . assertRequest 200 Nothing "other"
    ]
  where
    addA :: S.ByteString -> Request -> Request
    addA ct r = r {Wai.requestHeaders = ("Accept", ct) : filter (("Accept" ==) . fst) (Wai.requestHeaders r)}

--------------------------------------------------------------------------------

multipleFilter1App :: Application
multipleFilter1App = runApp $ do
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

test :: Test
test = testGroup "Application"
    [ helloWorldAllTest
    , methodFilterTest
    , httpVersionTest
    , rootFilterTest
    , restFilterTest
    , captureTest
    , queryTest
    , stopTest
    , acceptTest
    , multipleFilter1Test
    ]

