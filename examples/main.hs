{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Web.Apiary

import Network.Wai.Handler.Warp
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Logger

main :: IO ()
main = run 3000 . fmap runStdoutLoggingT . runApiaryT def $ do

    root $ do
        stdMethod GET . action_ $ do
            contentType "text/plain"
            lbs "Hello World."

    [capture|/number/:Int|] $ do

        stdMethod GET . action $ \i -> do
            contentType "text/plain"
            lbs . L.pack $ "GET " ++ show i

        stdMethod POST . action $ \i -> do
            contentType "text/plain"
            lbs . L.pack $ "POST " ++ show (i * 2)

    [capture|/div/:Double/:Double|] . action $ \(a,b) -> do
        when (b == 0) $ $logInfo "zero div."
        guard $ b /= 0
        contentType "text/plain"
        lbs . L.pack $ show (a / b)

    [capture|/static/:String|] $ do
        stdMethod GET . action $ \p -> do
            file p Nothing

-- | $ curl localhost:3000
-- Hello World.
-- $ curl -XPOST localhost:3000
-- 404 Page Notfound.
--
-- $ curl localhost:3000/number/1
-- GET 1
-- $ curl -XPOST localhost:3000/number/1
-- POST 2
-- $ curl -XPUT localhost:3000/number/1
-- 404 Page Notfound.
--
-- $ curl localhost:3000/div/10/2
-- 5.0
-- $ curl -XPOST localhost:3000/div/10/2
-- 5.0
-- $ curl localhost:3000/div/10/0
-- 404 Page Notfound.              # and logging
--
-- $ curl localhost:3000/static/main.hs # show file
-- $ curl localhost:3000/static/notfound.hs # show file
-- File not found
