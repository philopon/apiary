{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Web.Apiary
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Char8 as S

page :: Monad m => S.ByteString -> ActionT exts prms m ()
page tok = do
    contentType "text/html"
    appendBytes "<form method=\"POST\" action=\"/\">"
    appendBytes   "<input type=\"text\" name=\"str\"/>"
    appendBytes   "<input type=\"hidden\" name=\"_token\" value=\"" >> appendBytes tok >> appendBytes "\">"
    appendBytes   "<button type=\"submit\">submit</button>"
    appendBytes "</form>"

main :: IO ()
main = runApiaryWith (run 3000) (initSession def {sessionSecure = False}) def $ do
    root $ do  
        -- set valid session key.
        method GET . action $ csrfToken >>= page

        method POST . ([key|str|] =: pInt) . checkToken . action $
            param [key|str|] >>= showing

    -- set invalid session key.
    [capture|/token::S.ByteString|] . action $ param [key|token|] >>= page
