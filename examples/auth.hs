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
main = runApiaryWith (run 3000) (initSession def +> initAuth def {authSessionConfig = sc}) def $ do

    root . method GET $ do
        authorized [key|auth|] . action $ do
            liftIO $ print "a"
            contentType "text/html"

            bytes "your id: "
            appendShowing =<< param [key|auth|]
            appendBytes " \n<a href=\"/logout\">logout</a>"

        cookie [key|message|] (pOption pByteString) . action $ do
            liftIO $ print "b"
            contentType "text/html"

            reset
            maybe (return ()) (\m -> mapM_ appendBytes ["<h1>", m, "</h1>"]) =<< param [key|message|]

            authRoutes >>= mapM_ (\(n,r) -> do
                mapM_ appendBytes ["<div><a href=\"", r, "\">"]
                appendText n
                appendBytes "</a></div>")

            deleteCookie "message"

    [capture|/logout|] . method GET . action $ do
        liftIO $ print "c"
        authLogout
        setCookie def { setCookieName = "message", setCookieValue = "logout done, bye." }
        redirect "/"

    authHandler
