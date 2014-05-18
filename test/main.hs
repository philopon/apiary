{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Test.Framework
import Test.Framework.Providers.HUnit

import Web.Apiary
import Network.Wai
import Network.Wai.Test
import qualified Network.HTTP.Types as HTTP

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S

testReq :: String -> (Request -> IO ()) -> Test
testReq str f = 
    let (meth, other)   = break (== ' ') str
        (path, version) = break (== ' ') (tail other)
    in testCase str $ f (setPath (setVersion version $ (defaultRequest { requestMethod = S.pack meth })) (S.pack path))
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
    assertStatus 200 res
    assertContentType "text/plain" res
    assertBody body res

assertHtml200 :: L.ByteString -> Application -> Request -> IO ()
assertHtml200 body app req = flip runSession app $ do
    res <- request req
    assertStatus 200 res
    assertContentType "text/html" res
    assertBody body res

assert404 :: Application -> Request -> IO ()
assert404 app req = flip runSession app $ do
    res <- request req
    assertStatus 404 res
    assertContentType "text/plain" res
    assertBody "404 Page Notfound.\n" res

--------------------------------------------------------------------------------

helloWorldApp :: Application
helloWorldApp = runApiary def $ action $ do
    contentType "text/plain"
    lbs "hello"

helloWorldAllTest :: Test
helloWorldAllTest = testGroup "helloWorld" 
    [ testReq "GET /"    $ assertPlain200 "hello" helloWorldApp
    , testReq "GET /foo" $ assertPlain200 "hello" helloWorldApp
    , testReq "POST /"   $ assertPlain200 "hello" helloWorldApp
    ]

--------------------------------------------------------------------------------

methodFilterApp :: Application
methodFilterApp = runApiary def $ do
    method   "GET" . action $ contentType "text/plain" >> lbs "GET"
    stdMethod POST . action $ contentType "text/plain" >> lbs "POST"

methodFilterTest :: Test
methodFilterTest = testGroup "methodFilter"
    [ testReq "GET /"    $ assertPlain200 "GET" methodFilterApp
    , testReq "POST /"   $ assertPlain200 "POST" methodFilterApp
    , testReq "GET /foo" $ assertPlain200 "GET" methodFilterApp
    , testReq "DELETE /" $ assert404 methodFilterApp
    ]

--------------------------------------------------------------------------------

httpVersionApp :: Application
httpVersionApp = runApiary def $ do
    http09 . action $ contentType "text/plain" >> lbs "09"
    http10 . action $ contentType "text/plain" >> lbs "10"
    http11 . action $ contentType "text/plain" >> lbs "11"

httpVersionTest :: Test
httpVersionTest = testGroup "httpVersionFilter" 
    [ testReq "GET / HTTP/0.9" $ assertPlain200 "09" httpVersionApp
    , testReq "GET / HTTP/1.0" $ assertPlain200 "10" httpVersionApp
    , testReq "GET / HTTP/1.1" $ assertPlain200 "11" httpVersionApp
    ]

--------------------------------------------------------------------------------

rootFilterApp :: Application
rootFilterApp = runApiary def .  root . action $ do
    contentType "text/html"
    lbs "root"

rootFilterTest :: Test
rootFilterTest = testGroup "rootFilter"
    [ testReq "GET /"           $ assertHtml200 "root" rootFilterApp
    , testReq "POST /"          $ assertHtml200 "root" rootFilterApp
    , testReq "GET /neko"       $ assert404 rootFilterApp
    , testReq "GET /index.html" $ assertHtml200 "root" rootFilterApp
    ]

--------------------------------------------------------------------------------

captureApp :: Application
captureApp = runApiary def $ do
    [capture|/foo|]  . action $ contentType "text/plain" >> lbs "foo"
    [capture|/:Int|] . stdMethod GET . action $ \i -> contentType "text/plain" >> lbs (L.unwords ["Int", L.pack $ show i])
    [capture|/:Double|] . action $ \i -> contentType "text/plain" >> lbs (L.unwords ["Double", L.pack $ show i])
    [capture|/bar/:L.ByteString/:Int|] . action $ \s i -> contentType "text/plain" >> lbs (L.unwords [s, L.pack $ show i])
    [capture|/:L.ByteString|] . action $ \s -> contentType "text/plain" >> lbs (L.unwords ["fall", s])

captureTest :: Test
captureTest = testGroup "capture"
    [ testReq "GET /foo" $ assertPlain200 "foo" captureApp
    , testReq "GET /12"   $ assertPlain200 "Int 12" captureApp
    , testReq "GET /12.4" $ assertPlain200 "Double 12.4" captureApp
    , testReq "POST /12"  $ assertPlain200 "Double 12.0" captureApp
    , testReq "GET /bar"  $ assertPlain200 "fall bar" captureApp
    , testReq "GET /baz"  $ assertPlain200 "fall baz" captureApp
    , testReq "GET /bar/nyan/12" $ assertPlain200 "nyan 12" captureApp
    ]

--------------------------------------------------------------------------------

queryApp f g h = runApiary def $ do
    (f "foo" pInt)             . action $ \i -> contentType "text/plain" >> lbs (L.unwords ["foo", "Int", L.pack $ show i])
    (g "foo" pString)          . action $ \i -> contentType "text/plain" >> lbs (L.unwords ["foo", "String", L.pack $ show i])
    (h "foo" (pMaybe pString)) . action $ \i -> contentType "text/plain" >> lbs (L.unwords ["foo", "Maybe String", L.pack $ show i])

queryCheckApp :: Application
queryCheckApp = runApiary def $ do
    ("foo" ?: pInt)           . action $ contentType "text/plain" >> lbs (L.unwords ["foo", "Int"])
    ("foo" ?: pString)        . action $ contentType "text/plain" >> lbs (L.unwords ["foo", "String"])
    ("foo" ?: pMaybe pString) . action $ contentType "text/plain" >> lbs (L.unwords ["foo", "Maybe String"])

queryFirstTest :: Test
queryFirstTest = testGroup "First"
    [ testReq "GET /" $ assert404 app
    , testReq "GET /?foo" $ assertPlain200 "foo Maybe String Nothing" app
    , testReq "GET /?foo&foo=3" $ assertPlain200 "foo Maybe String Nothing" app
    , testReq "GET /?foo=12" $ assertPlain200 "foo Int 12" app
    , testReq "GET /?foo=a" $ assertPlain200 "foo String \"a\"" app
    , testReq "GET /?foo=12&foo=23" $ assertPlain200 "foo Int 12" app
    , testReq "GET /?foo=12&foo=b" $ assertPlain200 "foo String \"12\"" app
    ]
  where app = queryApp (=:) (=:) (=:)

queryOneTest :: Test
queryOneTest = testGroup "First"
    [ testReq "GET /" $ assert404 app
    , testReq "GET /?foo" $ assertPlain200 "foo Maybe String Nothing" app
    , testReq "GET /?foo&foo=3" $ assert404 app
    , testReq "GET /?foo=12" $ assertPlain200 "foo Int 12" app
    , testReq "GET /?foo=a" $ assertPlain200 "foo String \"a\"" app
    , testReq "GET /?foo=12&foo=23" $ assert404 app
    , testReq "GET /?foo=12&foo=b" $ assert404 app
    ]
  where app = queryApp (=!:) (=!:) (=!:)

queryOptionTest :: Test
queryOptionTest = testGroup "First"
    [ testReq "GET /" $ assertPlain200 "foo Int Nothing" app
    , testReq "GET /?foo" $ assertPlain200 "foo Maybe String Just Nothing" app
    , testReq "GET /?foo&foo=3" $ assertPlain200 "foo Maybe String Just Nothing" app
    , testReq "GET /?foo=12" $ assertPlain200 "foo Int Just 12" app
    , testReq "GET /?foo=a" $ assertPlain200 "foo String Just \"a\"" app
    , testReq "GET /?foo=12&foo=23" $ assertPlain200 "foo Int Just 12" app
    , testReq "GET /?foo=12&foo=b" $ assertPlain200 "foo String Just \"12\"" app
    ]
  where app = queryApp (=?:) (=?:) (=?:)

queryCheckTest :: Test
queryCheckTest = testGroup "First"
    [ testReq "GET /" $ assert404 app
    , testReq "GET /?foo" $ assertPlain200 "foo Maybe String" app
    , testReq "GET /?foo&foo=3" $ assertPlain200 "foo Maybe String" app
    , testReq "GET /?foo=12" $ assertPlain200 "foo Int" app
    , testReq "GET /?foo=a" $ assertPlain200 "foo String" app
    , testReq "GET /?foo=12&foo=23" $ assertPlain200 "foo Int" app
    , testReq "GET /?foo=12&foo=b" $ assertPlain200 "foo String" app
    ]
  where app = queryCheckApp

queryManyTest :: Test
queryManyTest = testGroup "First"
    [ testReq "GET /" $ assertPlain200 "foo Int []" app
    , testReq "GET /?foo" $ assertPlain200 "foo Maybe String [Nothing]" app
    , testReq "GET /?foo&foo=3" $ assertPlain200 "foo Maybe String [Nothing,Just \"3\"]" app
    , testReq "GET /?foo=12" $ assertPlain200 "foo Int [12]" app
    , testReq "GET /?foo=a" $ assertPlain200 "foo String [\"a\"]" app
    , testReq "GET /?foo=12&foo=23" $ assertPlain200 "foo Int [12,23]" app
    , testReq "GET /?foo=12&foo=b" $ assertPlain200 "foo String [\"12\",\"b\"]" app
    ]
  where app = queryApp (=*:) (=*:) (=*:)

querySomeTest :: Test
querySomeTest = testGroup "First"
    [ testReq "GET /" $ assert404 app
    , testReq "GET /?foo" $ assertPlain200 "foo Maybe String [Nothing]" app
    , testReq "GET /?foo&foo=3" $ assertPlain200 "foo Maybe String [Nothing,Just \"3\"]" app
    , testReq "GET /?foo=12" $ assertPlain200 "foo Int [12]" app
    , testReq "GET /?foo=a" $ assertPlain200 "foo String [\"a\"]" app
    , testReq "GET /?foo=12&foo=23" $ assertPlain200 "foo Int [12,23]" app
    , testReq "GET /?foo=12&foo=b" $ assertPlain200 "foo String [\"12\",\"b\"]" app
    ]
  where app = queryApp (=+:) (=+:) (=+:)


queryTest :: Test
queryTest = testGroup "query"
    [ queryFirstTest
    , queryOneTest
    , queryOptionTest
    , queryCheckTest
    , queryManyTest
    , querySomeTest
    ]

--------------------------------------------------------------------------------

multipleFilter1App :: Application
multipleFilter1App = runApiary def $ do
    root $ do
        stdMethod GET  . action $ contentType "text/plain" >> lbs "GET /"
        stdMethod POST . action $ contentType "text/html"  >> lbs "POST /"

    stdMethod DELETE . action $ contentType "text/plain" >> lbs "DELETE ANY"

multipleFilter1Test :: Test
multipleFilter1Test = testGroup "multiple test1: root, method"
    [ testReq "GET /index.html" $ assertPlain200 "GET /"      multipleFilter1App
    , testReq "POST /"          $ assertHtml200 "POST /"      multipleFilter1App
    , testReq "DELETE /"        $ assertPlain200 "DELETE ANY" multipleFilter1App 
    , testReq "PUT /"           $ assert404 multipleFilter1App
    ]

--------------------------------------------------------------------------------


main :: IO ()
main = defaultMain 
    [ helloWorldAllTest
    , methodFilterTest
    , httpVersionTest
    , rootFilterTest
    , captureTest
    , queryTest
    , multipleFilter1Test
    ]

