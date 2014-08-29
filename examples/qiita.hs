{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Network.Wai.Handler.Warp

main :: IO ()
main = server (run 3000) . runApiary def $ do
    [capture|/:Int|] . ("name" =: pLazyByteString) $ do
        method GET $ do
            action $ \age name -> do
                guard (age >= 18)
                contentType "text/html"
                bytes "<h1>Hello, "
                lazyBytes name
                bytes  "!</h1>\n"

            action $ \_ _ -> do
                contentType "text/html"
                bytes "R18\n"

        method POST . action $ \age name -> do
            liftIO $ print (age, name)

        method PUT . ("add" =?: pBool) . action $ \age name mbB -> do
            let bool = maybe False id mbB
            liftIO $ print (age, name, bool)

    root . method GET . action $ do
        contentType "text/html"
        bytes "<h1>Hello world!</h1>\n"
