{-# LANGUAGE OverloadedStrings #-}

import Test.Framework
import Test.Framework.Providers.HUnit

import Web.Apiary
import Network.Wai
import Network.Wai.Test

import qualified Data.ByteString.Lazy as L

getRoot :: Request
getRoot = defaultRequest

postRoot :: Request
postRoot = defaultRequest { requestMethod = "POST" }

deleteRoot :: Request
deleteRoot = defaultRequest { requestMethod = "DELETE" }

putRoot :: Request
putRoot = defaultRequest { requestMethod = "PUT" }

getIndexHtml :: Request
getIndexHtml = setPath defaultRequest "/index.html"

getNeko :: Request
getNeko = setPath defaultRequest "/neko"

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
    assertBody "404 Page Notfound." res

--------------------------------------------------------------------------------

helloWorldApp :: Application
helloWorldApp = runApiary def $ action $ do
    contentType "text/plain"
    lbs "hello"

helloWorldAllTest :: Test
helloWorldAllTest = testGroup "helloWorld" 
    [ testCase "GET /"     $ assertPlain200 "hello" helloWorldApp getRoot
    , testCase "GET /neko" $ assertPlain200 "hello" helloWorldApp getNeko
    , testCase "POST /"    $ assertPlain200 "hello" helloWorldApp postRoot
    ]

--------------------------------------------------------------------------------

methodFilterApp :: Application
methodFilterApp = runApiary def $ do
    method   "GET" . action $ contentType "text/plain" >> lbs "GET"
    stdMethod POST . action $ contentType "text/plain" >> lbs "POST"

methodFilterTest :: Test
methodFilterTest = testGroup "methodFilter"
    [ testCase "GET /"     $ assertPlain200 "GET" methodFilterApp getRoot
    , testCase "POST /"    $ assertPlain200 "POST" methodFilterApp postRoot
    , testCase "GET /neko" $ assertPlain200 "GET" methodFilterApp getNeko
    , testCase "DELETE /"  $ assert404 methodFilterApp deleteRoot
    ]

--------------------------------------------------------------------------------
rootFilterApp :: Application
rootFilterApp = runApiary def .  root . action $ do
    contentType "text/html"
    lbs "root"

rootFilterTest :: Test
rootFilterTest = testGroup "rootFilter"
    [ testCase "GET /"           $ assertHtml200 "root" rootFilterApp getRoot
    , testCase "POST /"          $ assertHtml200 "root" rootFilterApp postRoot
    , testCase "GET /neko"       $ assert404 rootFilterApp getNeko
    , testCase "GET /index.html" $ assertHtml200 "root" rootFilterApp getIndexHtml
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
    [ testCase "GET /index.html" $ assertPlain200 "GET /"       multipleFilter1App getIndexHtml
    , testCase "POST /"          $ assertHtml200 "POST /"      multipleFilter1App postRoot
    , testCase "DELETE /"        $ assertPlain200 "DELETE ANY" multipleFilter1App deleteRoot
    , testCase "PUT /"           $ assert404 multipleFilter1App putRoot
    ]

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain 
    [ helloWorldAllTest
    , methodFilterTest
    , rootFilterTest
    , multipleFilter1Test
    ]

