{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import Web.Apiary
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp

main :: IO ()
main = runApiaryWith (run 3000) (initSession $embedDefaultKeyConfig { sessionSecure = False } ) def $ do
    root . method GET $ do
        session [key|test|] (pOne pLazyByteString) . action $ do
            lazyBytes "session: "
            appendLazyBytes =<< param [key|test|]

        action $ do
            setSession "test" "nyan"
            lazyBytes "set session"
