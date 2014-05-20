{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Web.Apiary.Cookie
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8      as S

main :: IO ()
main = run 3000 . runApiary def $ do

    [capture|/:Int|] . action $ \s -> do
        setCookie def { setCookieName = "param", setCookieValue = S.pack $ show s }
        setCookie def { setCookieName = "dog", setCookieValue = "bowwow" }
        lbs "lucky cookie"

    [capture|/delete|] . action $ do
        deleteCookie "param"
        deleteCookie "dog"
        lbs "unlucky cookie"

    root . cookie "param" (pFirst pInt) 
         . cookie "dog"   (pFirst pLazyByteString) . action $ \p d -> do
             contentType "text/plain"
             lbs $ L.unlines [L.pack $ show p, d]
