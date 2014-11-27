{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Network.Wai.Handler.Warp

main :: IO ()
main = runApiary (run 3000) def $ do
    [capture|/age::Int|] . ([key|name|] =: pLazyByteString) $ do
        method GET $ do
            action $ do
                (age, name) <- [params|age, name|]
                guard (age >= 18)
                contentType "text/html"
                bytes "<h1>Hello, "
                appendLazyBytes name
                appendBytes  "!</h1>\n"

            action $ do
                contentType "text/html"
                appendBytes "R18\n"

        method POST . action $ do
            (age, name) <- [params|age, name|]
            liftIO $ print (age, name)

        method PUT . ([key|add|] =?!: False) . action $ do
            (age, name, add) <- [params|age, name, add|]
            liftIO $ print (age, name, add)

    root . method GET . action $ do
        contentType "text/html"
        bytes "<h1>Hello world!</h1>\n"
