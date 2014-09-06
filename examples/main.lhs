> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TemplateHaskell #-}
> 
> import Web.Apiary
> 
> import Network.Wai.Handler.Warp
> import Control.Monad.Reader
> import qualified Data.ByteString.Lazy.Char8 as L
> 
> main :: IO ()
> main = runApiary def (run 3000)$ do

Apiary has 2 Monads, Apiary and Action.
Apiary is filtering Request, Action is processing request.

'root' filter catchs root like path, in default, 
It's 'host:port', 'host:port/', 'host:port/index.htm', 'host:port/index.html'.
It can configure ApiaryConfig.

>     root $ do

Then 'method' filter apply and execute action. 
So only root like path and GET method is processed.

>         method GET . action $ do

Set content-type. default response header can configure by ApiaryConfig.

>             contentType "text/plain"

Set response body.

>             bytes "Hello World."

Capture filter filtering 'path' and read ':Type'.

>     [capture|/number/:Int|] $ do

You can get captured elements by action (without underscore) function.

>         method GET . action $ \i -> do
>             contentType "text/plain"
>             bytes "GET " >> showing i

>         method POST . action $ \i -> do
>             contentType "text/plain"
>             bytes "POST " >> showing (i * 2)

Multiple type capturing. can get by tuple.

>     [capture|/div/:Double/:Double|] . action $ \a b -> do
>         when (b == 0) $ liftIO $ putStrLn "zero div."

You can use MonadPlus instance. when b == 0, 404 page not found.

>         guard $ b /= 0
>         contentType "text/plain"
>         showing (a / b)

>     [capture|/static/:String|] $ do
>         method GET . action $ \p -> do

Static file provider. content-type auto detected by extension.

>             file p Nothing

when execute 'stop' action, send current status and drop after actions.

>     [capture|/stop/:Int|] . action $ \i -> do
>         contentType "text/plain"
>         bytes "stop the handler!\n"
>         when (odd i) $ stop
>         bytes "don't stop handler...\n"
>

filters can freely nesting.

>     [capture|/greeting/:L.ByteString|] . 
>         ("first" =: pLazyByteString) . 
>         ("last"  =: pLazyByteString) . [act|200 .txt|] $ \greed first last_ -> do
>             lazyBytes greed >> bytes "!! "
>             lazyBytes first >> char ' ' >> lazyBytes last_

$ curl localhost:3000
Hello World.
$ curl -XPOST localhost:3000
404 Page Notfound.

$ curl localhost:3000/number/1
GET 1
$ curl -XPOST localhost:3000/number/1
POST 2
$ curl localhost:3000/number/num
404 Page Notfound.
$ curl -XPUT localhost:3000/number/1
404 Page Notfound.

$ curl localhost:3000/div/10/2
5.0
$ curl -XPOST localhost:3000/div/10/2
5.0
$ curl localhost:3000/div/10/0
404 Page Notfound.              # and show stdout

$ curl localhost:3000/static/main.lhs # show file
$ curl localhost:3000/static/notfound.hs # show file
File not found

$ curl localhost:3000/stop/1
stop the handler!
$ curl localhost:3000/stop/2
stop the handler!
don't stop handler...

curl "localhost:3000/greeting/hi?first=John&last=Smith"
hi!! John Smith
