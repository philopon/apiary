{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Web.Apiary.WebSockets
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Control.Concurrent

main :: IO ()
main = run 3000 . runApiary def $ do
    [capture|/:Int|] . actionWithWebSockets servApp $ \_ -> do
        file "websockets.html" Nothing

servApp :: Int -> PendingConnection -> IO ()
servApp st pc = do
    c <- acceptRequest pc
    go c st
  where
    go c 10 = sendClose c ("Close" :: T.Text)
    go c i = do
        sendTextData c (T.pack $ show i)
        liftIO $ putStrLn "send"
        threadDelay (10 ^ (6 :: Int))
        go c (succ i)
