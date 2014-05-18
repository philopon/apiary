{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- for implicit signature
{-# LANGUAGE FlexibleContexts #-} -- for explicit signature

import Web.Apiary
import Web.Apiary.Cookie
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8      as S

main :: IO ()
main = withCookie def $ run 3000 . runApiary def $ do

    [capture|/:S.ByteString|] . action $ \s -> do
        setCookie (def { setCookieName = "param", setCookieValue = s })
        setCookie (def { setCookieName = "dog", setCookieValue = "bowwow" })
        contentType "text/plain"
        lbs "lucky cookie."

    root . cookie "param" (pFirst pInt) 
         . cookie "dog" (pFirst pLazyByteString) . action $ \p d -> do
             contentType "text/plain"
             lbs $ L.unlines [L.pack $ show p, d]


