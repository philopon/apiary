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
main = serverWith (initSession def +> initAuth def {authSessionConfig = sc}) (run 3000) . runApiary def $ do

    root . method GET $ do
        authorized [key|auth|] . action $ do
            contentType "text/html"

            bytes "your id: "
            showing =<< param [key|auth|]
            bytes " \n<a href=\"/logout\">logout</a>"

        cookie [key|message|] (pOption pByteString) . action $ do
            contentType "text/html"

            maybe (return ()) (\m -> mapM_ bytes ["<h1>", m, "</h1>"]) =<< param [key|message|]

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
