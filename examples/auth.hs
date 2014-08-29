{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Web.Apiary.Cookie
import Web.Apiary.Authenticate
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp

sc :: SessionConfig
sc = def { sessionPath = Just "/", sessionSecure = False }

main :: IO ()
main = server (run 3000) . runApiaryWith (initSession def +> initAuth def {authSessionConfig = sc}) def $ do

    root . method GET $ do
        authorized . action $ \s -> do
            contentType "text/html"

            bytes "your id: "
            showing s
            bytes " \n<a href=\"/logout\">logout</a>"

        cookie "message" (pOption pByteString) . action $ \mbmsg -> do
            contentType "text/html"

            maybe (return ()) (\m -> mapM_ bytes ["<h1>", m, "</h1>"]) mbmsg

            authRoutes >>= mapM_ (\(n,r) -> do
                mapM_ bytes ["<div><a href=\"", r, "\">"]
                text n
                bytes "</a></div>")

            deleteCookie "message"

    [capture|/logout|] . method GET . action $ do
        authLogout
        setCookie def { setCookieName = "message", setCookieValue = "logout done, bye." }
        redirect "/"

    authHandler
