> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE DataKinds #-}
> 
> import Web.Apiary
> import Web.Apiary.Logger
> import Network.Wai.Handler.Warp
> import qualified Data.ByteString as S
> 
> main :: IO ()
> main = runApiaryWith (run 3000) (initLogger def) def $ do

                                  ~~~~~~~~~~~~~~~~
                                  use logger extension



>     [capture|/hello/first::S.ByteString[first name of client.]|]

      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      route capture QQ.

      name::Type[document] = parameter capture
      **name               = consume greedy



>       . ([key|last|] ?? "last name of client" =?: pByteString)

          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          query parameter filter.
      
          key QQ is simple helper: `[key|foo|] == (Proxy :: Proxy "foo")`.
          (??) is add document to query parameter.


>       . method GET $ do

          ~~~~~~~~~~
          method filter. you can use non-standard method by
          string literal (example: method "HOGE").



>         accept "text/plain"

          ~~~~~~~~~~~~~~~~~~
          accept filter: filter by Accept header and set content-type of response.



>             . document "plain hello page."

                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                add document to action.



>             . action $ do

                ~~~~~~
                splice ActionT monad to filter.



>                 logging "text page is accessed.\n"

                  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  extension action.



>                 helloAction

>         accept "text/html"
>             . document "html hello page."
>             . action $ do
>                 logging "html page is accessed.\n"
>                 bytes "<h1>"
>                 helloAction
>                 appendBytes "</h1>"
> 
>     [capture|/api|] . document "api documentation" . action $ do
>         logging "api documentation page is accessed.\n"
>         defaultDocumentationAction def

          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          auto generated api documentation action.




> helloAction :: Members ["first" := S.ByteString, "last" := Maybe S.ByteString] prms

                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 prms must have "first" :: S.ByteString
                 and "last" :: Maybe S.ByteString parameters.



>             => ActionT exts prms IO ()
> helloAction = do
>     (f, l) <- [params|first, last|]

                ~~~~~~~~~~~~~~~~~~~~
                get parameters. equals to `do { f <- param [key|first|];
                                                l <- param [key|last|] }`



>     appendBytes "Hello, "
>     appendBytes f
>     maybe (return ()) (\a -> appendChar ' ' >> appendBytes a) l
