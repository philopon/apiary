{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import Web.Apiary
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp

main :: IO ()
main = server (run 3000) . runApiaryWith (initSession $embedDefaultKeyConfig { sessionSecure = False } ) def $ do
    root . method GET $ do
        session "test" (pOne pLazyByteString) . action $ \test -> do
            lazyBytes "session: "
            lazyBytes test

        action $ do
            setSession "test" "nyan"
            lazyBytes "set session"

