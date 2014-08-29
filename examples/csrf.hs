{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Web.Apiary
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Char8 as S

page :: Monad m => S.ByteString -> ActionT exts m ()
page tok = do
    contentType "text/html"
    bytes "<form method=\"POST\" action=\"/\">"
    bytes   "<input type=\"text\" name=\"str\"/>"
    bytes   "<input type=\"hidden\" name=\"_token\" value=\"" >> bytes tok >> bytes "\">"
    bytes   "<button type=\"submit\">submit</button>"
    bytes "</form>"

main :: IO ()
main = server (run 3000) . runApiaryWith (initSession def {sessionSecure = False}) def $ do
    root $ do  
        -- set valid session key.
        method GET . action $ csrfToken >>= page

        method POST . ("str" =: pInt) . checkToken . action $ showing

    -- set invalid session key.
    [capture|/:S.ByteString|] . action $ page
