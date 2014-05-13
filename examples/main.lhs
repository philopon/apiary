> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TemplateHaskell #-}
> 
> import Web.Apiary
> 
> import Network.Wai.Handler.Warp
> import Control.Monad.Reader
> import qualified Data.ByteString.Lazy.Char8 as L
> import Control.Monad.Logger
> 
> main :: IO ()

'runApiaryT' create ApplicationM m, when m == IO, equals Application.
and you can use Monad Transformers with fmap(but cannot share state, so StateT not work.).

> main = run 3000 . fmap runStdoutLoggingT . runApiaryT def $ do

apiary has 2 Monads, ApiaryT and ActionT.
ApiaryT is filtering Request, ActionT is processing request.

'root' filter catchs root like path, in default, it's 'host:port', 'host:port/', 'host:port/index.htm', 'host:port/index.html'. it can configure ApiaryConfig.

>     root $ do

then 'stdMethod' filter apply and execute action. so only root like path and GET method is processed.

>         stdMethod GET . action_ $ do

set content-type. default response header can configure by ApiaryConfig.

>             contentType "text/plain"

set response body.

>             lbs "Hello World."

capture filter filtering 'path' and read ':Type'.

>     [capture|/number/:Int|] $ do

you can get captured elements by action (without underscore) function.

>         stdMethod GET . action $ \i -> do
>             contentType "text/plain"
>             lbs . L.pack $ "GET " ++ show i

>         stdMethod POST . action $ \i -> do
>             contentType "text/plain"
>             lbs . L.pack $ "POST " ++ show (i * 2)

multiple type capturing. can get by tuple.

>     [capture|/div/:Double/:Double|] . action $ \(a,b) -> do
>         when (b == 0) $ $logInfo "zero div."

you can use MonadPlus instance. when b == 0, 404 page not found.

>         guard $ b /= 0
>         contentType "text/plain"
>         lbs . L.pack $ show (a / b)

>     [capture|/static/:String|] $ do
>         stdMethod GET . action $ \p -> do

static file provider. content-type auto detected by extension.

>             file p Nothing

$ curl localhost:3000
Hello World.
$ curl -XPOST localhost:3000
404 Page Notfound.

$ curl localhost:3000/number/1
GET 1
$ curl -XPOST localhost:3000/number/1
POST 2
$ curl -XPUT localhost:3000/number/1
404 Page Notfound.

$ curl localhost:3000/div/10/2
5.0
$ curl -XPOST localhost:3000/div/10/2
5.0
$ curl localhost:3000/div/10/0
404 Page Notfound.              # and logging

$ curl localhost:3000/static/main.lhs # show file
$ curl localhost:3000/static/notfound.hs # show file
File not found
