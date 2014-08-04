{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import Web.Apiary
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = withSession $embedDefaultKeyConfig { sessionSecure = False } $ run 3000 . runApiary def $ do
    root . method GET $ do
        session "test" (pOne pLazyByteString) . action $ \test -> do
            lbs $ "session: " `L.append` test

        action $ do
            setSession "test" "nyan"
            lbs "set session"

