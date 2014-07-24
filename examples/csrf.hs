{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Web.Apiary
import Web.Apiary.ClientSession
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S

page :: Monad m => S.ByteString -> ActionT m ()
page tok = do
    contentType "text/html"
    lbs . L.unlines $ 
        [ "<form method=\"POST\" action=\"/\">"
        , "<input type=\"text\" name=\"str\"/>"
        , L.concat ["<input type=\"hidden\" name=\"_token\" value=\"", L.fromStrict tok, "\">"]
        , "<button type=\"submit\">submit</button>"
        , "</form>"
        ]


main :: IO ()
main = withSession def { sessionSecure = False } $ run 3000 . runApiary def $ do
    root $ do  
        method GET . action $ csrfToken >>= page

        method POST . ("str" =: pInt) . checkToken . action $ \i -> do
            lbs $ L.pack (show i)

    [capture|/:S.ByteString|] . action $ page
