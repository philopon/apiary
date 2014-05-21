Apiary [![Build Status](https://travis-ci.org/philopon/apiary.svg?branch=master)](https://travis-ci.org/philopon/apiary)
====
Simple web framework inspired by scotty.

Feature
----
* small core library.
* type level and nestable router.

Install
----
```bash
cabal sandbox init
cabal install apiary warp
```

Hello World
----
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Network.Wai.Handler.Warp

main :: IO ()
main = run 3000 . runApiary def $ do
    action $ do
        lbs "Hello World!\n"
```
display "Hello World!" in any path, parameter.

routing
----
routing functions can nesting freely. and arguments are stocked on type level.

this is routing monad named Apiary.

```haskell
data Apiary c a
```

first argument of Apiary is stocked arguments.

initial arguments is empty, so type of runApiary is

```haskell
runApiary :: ApiaryConfig -> Apiary '[] a -> Application
```

WARNING: type signature of this section is pseudo code for explanation, but value is haskell code.

### capture

capture QuasiQuote: simple path router. :Type = read as Type, otherwise matching string.

```haskell
[capture|/path/:Int|] :: Apiary (xs `Snoc` Int) b -> Apiary xs b
```

when first path == "path" and second path is readable as Int, filter successed.

### query parameter

you can route using query function.

```haskell
query :: (Query a, Strategy w) => ByteString -> Proxy (w a) -> Apiary (SNext w as a) b -> Apiary as b 
```

example:

```haskell
query "key" (Proxy :: Proxy First Int)
```

Strategy chooses query getting strategy. Predefined strategy is:

* First(get first parameter)
* One(get one parameter)
* Option(get optional parameter)
* Many(get zero or more parameters)
* Some(get one or more parameters)
* Check(check parameter exists).

query function is little verbose, so there are shortcut functions. so, you can write:

```haskell
("key" =: pDouble)          -- get first Double parameter
("key" =!: pInt)            -- get one Int parameter
("key" =?: pMaybe pString)  -- get optional String parameter which can ommit value.
("key" ?: ())               -- check parameter exists.(not type checked)
("key" =*: pText)           -- get zero or more Text parameters.
("key" =*: pLazyByteString) -- get one or more lazy ByteString parameters.
```

### filter only routers
these router is not modify arguments.

#### root
root router is match only root like path configured by ApiaryConfig. 
default value is ["", "/", "index.htm", "index.html"]

#### other
method, stdMethod, ssl

### function, function
low level filter functions.

Action
----
Action monad is use define create response.

splice Action to Apiary, using action function.

```haskell
action :: Monad m => Fn c (Action ()) -> Apiary c () 
```

Fn c is apply stocked arguments.

so, when stock [] (initlal state) then ActionT m (),
         stock [Int, Double]      then Int -> Double -> ActionT m ()

you can use some getter(raw request, query string, request header),
setter(status, response header, response body) 
and interupt and return current response(stop function).

Example
----
```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = run 3000 . runApiary def $ do
    [capture|/:Int|] $ do
        -- freely
        ("query" =: pString) $ do
            -- nestable
            ("mbQuery" =?: pDouble) $ do
                -- filters
                stdMethod GET . action $ \int query mbQuery -> do
                    contentType "text/plain"
                    lbs $ L.unlines $ "GET" : map L.pack [show int, query, show mbQuery]

                stdMethod DELETE . action $ \_ _ _ -> do
                    lbs "DELETE!\n"

            ("mbQuery" =: pLazyByteString) $ do

                action $ \_ _ mbQuery -> do
                    contentType "text/plain"
                    lbs . L.unwords $ [mbQuery, "is not Double.\n"]

    -- no filter: default action
    action $ do
        lbs "Hello World!\n"
```

```bash
$ curl localhost:3000
Hello World!
$ curl 'localhost:3000/12?query=test'
GET
12
test
Nothing
$ curl -XDELETE 'localhost:3000/12?query=test'
DELETE!
$ curl 'localhost:3000/12?query=test&mbQuery=42'
GET
12
test
Just 42.0
$ curl 'localhost:3000/12?query=test&mbQuery=xxx'
xxx is not Double.
```
