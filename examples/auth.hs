{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Web.Apiary.Cookie
import Web.Apiary.Authenticate
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Encoding as T

main :: IO ()
main = withSession def { sessionPath = Just "/", sessionSecure = False} $ withAuth def $ run 3000 . runApiary def $ do

    root . stdMethod GET $ do
        authorized . action $ \s -> do
            contentType "text/html"
            lbs $ L.unwords ["your id:", L.pack $ show s, "\n<a href=\"/logout\">logout</a>"]

        cookie "message" (pOption pByteString) . action $ \mbmsg -> do
            contentType "text/html"
            let elm = concatMap (\(n,r) -> ["<div><a href=\"", r, "\">", T.encodeUtf8 n, "</a></div>"]) authRoutes
            lbs $ L.fromChunks $ maybe [] (\m -> ["<h1>", m, "</h1>"]) mbmsg ++ elm
            deleteCookie "message"

    [capture|/logout|] . stdMethod GET . action $ do
        authLogout
        setCookie def { setCookieName = "message", setCookieValue = "logout done, bye." }
        redirect "/"

    authHandler
