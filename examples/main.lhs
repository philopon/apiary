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
And you can use Monad Transformers with fmap(but cannot share state, so StateT not work.).

> main = run 3000 . runApiaryT def runStdoutLoggingT $ do

Apiary has 2 Monads, ApiaryT and ActionT.
ApiaryT is filtering Request, ActionT is processing request.

'root' filter catchs root like path, in default, 
It's 'host:port', 'host:port/', 'host:port/index.htm', 'host:port/index.html'.
It can configure ApiaryConfig.

>     root $ do

Then 'stdMethod' filter apply and execute action. 
So only root like path and GET method is processed.

>         stdMethod GET . action $ do

Set content-type. default response header can configure by ApiaryConfig.

>             contentType "text/plain"

Set response body.

>             lbs "Hello World."

Capture filter filtering 'path' and read ':Type'.

>     [capture|/number/:Int|] $ do

You can get captured elements by action (without underscore) function.

>         stdMethod GET . action $ \i -> do
>             contentType "text/plain"
>             lbs . L.pack $ "GET " ++ show i

>         stdMethod POST . action $ \i -> do
>             contentType "text/plain"
>             lbs . L.pack $ "POST " ++ show (i * 2)

Multiple type capturing. can get by tuple.

>     [capture|/div/:Double/:Double|] . action $ \a b -> do
>         when (b == 0) $ $logInfo "zero div."

You can use MonadPlus instance. when b == 0, 404 page not found.

>         guard $ b /= 0
>         contentType "text/plain"
>         lbs . L.pack $ show (a / b)

>     [capture|/static/:String|] $ do
>         stdMethod GET . action $ \p -> do

Static file provider. content-type auto detected by extension.

>             file p Nothing

when execute 'stop' action, send current status and drop after actions.

>     [capture|/stop/:Int|] . action $ \i -> do
>         contentType "text/plain"
>         lbs "stop the handler!"
>         when (odd i) $ stop
>         lbs "don't stop handler..."
>

filters can freely nesting.

>     [capture|/greeting/:L.ByteString|] . 
>         ("first" =: pLazyByteString) . 
>         ("last"  =: pLazyByteString) . action $ \greed first last -> do
>             contentType "text/plain"
>             lbs $ L.unwords [greed `L.append` "!!", first, last]

$ curl localhost:3000
Hello World.
$ curl -XPOST localhost:3000
404 Page Notfound.

$ curl localhost:3000/number/1
GET 1
$ curl -XPOST localhost:3000/number/1
POST 2
curl localhost:3000/number/0x24
GET 36
$ curl localhost:3000/number/num
404 Page Notfound.
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

$ curl localhost:3000/stop/1
stop the handler!
$ curl localhost:3000/stop/2
don't stop handler...

curl "localhost:3000/greeting/hi?first=John&last=Smith"
hi!! John Smith
