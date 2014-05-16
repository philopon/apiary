[![Build Status](https://secure.travis-ci.org/philopon/apiary.png)](https://travis-ci.org/philopon/apiary)

Apiary
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

this is routing monad named ApiaryT.

```haskell
data ApiaryT c m a
```

first argument of ApiaryT is stocked arguments.

initial arguments is empty, so type of runApiary is

```haskell
runApiary :: ApiaryConfig IO -> ApiaryT '[] IO a -> Application
```

WARNING: type signature of this section is pseudo code for explanation, but value is haskell code.

### capture

capture QuasiQuote: simple path router. :Type = read as Type, otherwise matching string.

```haskell
[capture|/path/:Int|] :: ApiaryT (xs `Snoc` Int) m b -> ApiaryT xs m b
```

when first path == "path" and second path is readable as Int, filter successed.

### query parameter

you can route by querySome, querySome', queryFirst, queryFirst'.

```haskell
querySome   "query" :: ApiaryT (xs `Snoc` [Maybe ByteString]) m b
querySome'  "query" :: ApiaryT (xs `Snoc` [      ByteString]) m b
queryFirst  "query" :: ApiaryT (xs `Snoc`  Maybe ByteString ) m b
queryFirst' "query" :: ApiaryT (xs `Snoc`        ByteString ) m b
```

and query getter (always success matching, so useful get optional query parameter)

```haskell
queryMany        "query" :: ApiaryT (xs `Snoc` Maybe [Maybe ByteString]) m b
queryMany'       "query" :: ApiaryT (xs `Snoc` Maybe [      ByteString]) m b
maybeQueryFirst  "query" :: ApiaryT (xs `Snoc` Maybe  Maybe ByteString ) m b
maybeQueryFirst' "query" :: ApiaryT (xs `Snoc` Maybe        ByteString ) m b
```

hoge' function not allow empty query value.

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
ActionT monad is use define create response.

splice ActionT to ApiaryT, using action function.

```haskell
action :: Monad m => Fn c (ActionT m ()) -> ApiaryT c m () 
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
        queryFirst' "query" . maybeQueryFirst' "mbQuery" $ do
            -- nestable
            stdMethod GET . action $ \int query mbQuery -> do
                contentType "text/plain"
                lbs $ L.unlines $ "GET" : map (L.pack) [show int, show query, show mbQuery]

            stdMethod DELETE . action $ \_ _ _ -> do
                lbs "DELETE!\n"
    action $ do
        lbs "Hello World!\n"
```

