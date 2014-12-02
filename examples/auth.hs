{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Web.Apiary.Cookie
import Web.Apiary.Authenticate
import Web.Apiary.Session.ClientSession
import Network.Wai.Handler.Warp

sc :: ClientSessionConfig
sc = def { csCookiePath = Just "/", csCookieSecure = False }

main :: IO ()
main = runApiaryWith (run 3000) (initClientSession pOpenId sc +> initAuth def) def $ do

    root . method GET $ do
        authorized . action $ do
            contentType "text/html"

            bytes "your id: "
            appendShowing =<< param [key|auth|]
            appendBytes " \n<a href=\"/logout\">logout</a>"

        cookie [key|message|] (pOption pByteString) . action $ do
            contentType "text/html"

            reset
            maybe (return ()) (\m -> mapM_ appendBytes ["<h1>", m, "</h1>"]) =<< param [key|message|]

            authRoutes >>= mapM_ (\(n,r) -> do
                mapM_ appendBytes ["<div><a href=\"", r, "\">"]
                appendText n
                appendBytes "</a></div>")

            deleteCookie "message"

    [capture|/logout|] . method GET . action $ do
        authLogout
        setCookie def { setCookieName = "message", setCookieValue = "logout done, bye." }
        redirect "/"

    authHandler
