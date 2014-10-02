{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import Web.Apiary
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp

main :: IO ()
main = serverWith (initSession $embedDefaultKeyConfig { sessionSecure = False } ) (run 3000) . runApiary def $ do
    root . method GET $ do
        session [key|test|] (pOne pLazyByteString) . action $ do
            lazyBytes "session: "
            lazyBytes =<< param [key|test|]

        action $ do
            setSession "test" "nyan"
            lazyBytes "set session"
