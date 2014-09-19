Apiary [![Build Status](https://travis-ci.org/philopon/apiary.svg?branch=master)](https://travis-ci.org/philopon/apiary) [![Hackage](https://budueba.com/hackage/apiary)](https://hackage.haskell.org/package/apiary)
====

Simple web framework inspired by scotty.

Feature
----
* small core library.
* high performance(benchmark: https://github.com/philopon/apiary-benchmark)
* type level and nestable router.
* auto generate API documents.

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
main = server (run 3000). runApiary def $ do
    action $ do
        bytes "Hello World!\n"
```
display "Hello World!" at any path, parameter.

routing
----
routing functions can nesting freely. and arguments are stocked on type level.

this is routing monad named Apiary.

```haskell
data ApiaryT exts prms actM m a
```

ext is global state for extension.

prms is stocked arguments.

actM is base monad of ActionT. it's normally IO.

initial prms is empty, so type of runApiary is

```haskell
runApiary :: ApiaryConfig -> ApiaryT '[] '[] IO m () -> m Application
```

this is little annoying, so use this type alias in this file.

```haskell
type Apiary prms a = ApiaryT '[] prms IO IO a
```

### capture

capture QuasiQuote: simple path router. :Type = read as Type, otherwise matching string.

```haskell
[capture|/path/:Int|] :: Apiary (Int ': xs) () -> Apiary xs ()
```

when first path == "path" and second path is readable as Int, filter successed.

### query parameter

you can route using query function.

```haskell
query :: (ReqParam a, Strategy w) => QueryKey -> w a -> Apiary (SNext w prms a) () -> Apiary  prms ()
```

example:

```haskell
query "key" (First :: First Int)
```

Strategy chooses query getting strategy. Predefined strategy is:

* First(get first parameter)
* One(get one parameter)
* Option(get optional parameter)
* Option(get optional parameter with default value)
* Many(get zero or more parameters)
* Some(get one or more parameters)
* LimitSome(get one or more parameters with upper bound)
* Check(check parameter exists).

query function is little verbose, so there are shortcut functions. so, you can write:

```haskell
("key" =: pDouble)              -- get first Double parameter
("key" =!: pInt)                -- get one Int parameter
("key" =?: pString)             -- get optional String parameter which can ommit value.
("key" =?!: ("foo" :: String))  -- get optional String parameter, default value is "foo".
("key" ?: ())                   -- check parameter exists.(not type checked)
("key" =*: pText)               -- get zero or more Text parameters.
("key" =*: pLazyByteString)     -- get one or more lazy ByteString parameters.
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
action :: Monad m => Fn prms (ActionT exts actM ()) -> ApiaryT exts prms actM m () 
```

this is little annoying, so use this type alias in this file.

```haskell
type Action a = ActionT '[] IO a
```

Fn prms is apply stocked arguments.

so, when stock \[\] (initlal state) then ActionT m (),  
stock [Double, Int]      then Int -> Double -> ActionT m ()

you can use some getter(raw request, query string, request header),  
setter(status, response header, response body)  
and interupt and return current response(stop function).

extensions
---
you can use simple extension api of apiary.

* initialize in ApiaryT base monad.
* add grobal environment.
* access grobal environment from ApiaryT and ActionT.

type of extension initializer is:

```haskell
newtype Initializer m i o = Initializer { unInitializer :: Extensions i -> m (Extensions o) }

data Extensions exts where
  NoExtension  :: Extensions `[]
  AddExtension :: e -> Extensions exts -> Extensions (e ': exts)
```

you can construct initializer using

```haskell
initializer :: Monad m => m e -> Initializer m i (e ': i)
```

examples:

```haskell
-- add global environment only

newtype Limit = Limit Int

initLimit = initializer (return $ Limit 18)

-- add global environment and print massage on initialize.

initLimitMsg lim = initializer (putStrLn "initialize limit" >> return (Limit lim)
```

you can use extensions using

```
runApiaryWith :: Monad m => Initializer m `[] exts -> ApiaryConfig -> ApiaryT exts `[] IO m () -> m Application
```

and you can combine two initializer using `(+>)`.

Example
----
```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = server (run 3000) . runApiary def $ do
    [capture|/:Int|] $ do
        -- freely
        ("query" =: pByteString) $ do
            -- nestable
            ("mbQuery" =?: pDouble) $ do
                -- filters
                method GET . action $ \int query mbQuery -> do
                    contentType "text/plain"
                    bytes "GET\n"
                    showing int
                    bytes query
                    showing mbQuery

                method DELETE . action $ \_ _ _ -> do
                    bytes "DELETE!\n"

            ("mbQuery" =: pLazyByteString) $ do

                action $ \_ _ mbQuery -> do
                    contentType "text/plain"
                    lazyBytes mbQuery
                    bytes " is not Double.\n"

    -- no filter: default action
    action $ do
        bytes "Hello World!\n"
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
