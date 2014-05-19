{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = run 3000 . runApiary def $ do
    [capture|/:Int|] . ("name" =: pLazyByteString) $ do
        stdMethod GET $ do
            action $ \age name -> do
                guard (age >= 18)
                contentType "text/html"
                lbs . L.concat $ ["<h1>Hello, ", name, "!</h1>\n"]

            action $ \_ _ -> do
                contentType "text/html"
                lbs "R18\n"

        stdMethod POST . action $ \age name -> do
            liftIO $ print (age, name)

        stdMethod PUT . ("add" =?: pBool) . action $ \age name mbB -> do
            let bool = maybe False id mbB
            liftIO $ print (age, name, bool)

    root . stdMethod GET . action $ do
        contentType "text/html"
        lbs "<h1>Hello world!</h1>\n"
