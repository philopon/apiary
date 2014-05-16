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

    [capture|/:String|] . action $ \s -> do
        setCookie (def { setCookieName = "param", setCookieValue = S.pack s })
        setCookie (def { setCookieName = "dog", setCookieValue = "bowwow" })
        contentType "text/plain"
        lbs "lucky cookie."

    root $ action splittedAction

splittedAction :: (Monad m, HasCookie) => ActionT m ()
splittedAction = do
    s <- getCookie' "param"
    p <- getCookie' "dog"
    contentType "text/plain"
    lbs $ L.unlines [L.fromStrict s, L.fromStrict p]
