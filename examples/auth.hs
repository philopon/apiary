{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Web.Apiary.Cookie
import Web.Apiary.Authenticate
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = withSession def { sessionPath = Just "/", sessionSecure = False} $ withAuth def $ run 3000 . runApiary def $ do

    root . method GET $ do
        authorized . action $ \s -> do
            contentType "text/html"

            bytes "your id: "
            lazyBytes . L.pack $ show s
            bytes " \n<a href=\"/logout\">logout</a>"

        cookie "message" (pOption pByteString) . action $ \mbmsg -> do
            contentType "text/html"

            maybe (return ()) (\m -> mapM_ bytes ["<h1>", m, "</h1>"]) mbmsg

            forM_ authRoutes $ \(n,r) -> do
                mapM_ bytes ["<div><a href=\"", r, "\">"]
                text n
                bytes "</a></div>"

            deleteCookie "message"

    [capture|/logout|] . method GET . action $ do
        authLogout
        setCookie def { setCookieName = "message", setCookieValue = "logout done, bye." }
        redirect "/"

    authHandler
