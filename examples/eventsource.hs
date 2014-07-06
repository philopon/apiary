{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Web.Apiary.EventSource
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder.Char8
import qualified Data.Text as T
import Control.Concurrent

main :: IO ()
main = do
    chan <- liftIO newChan
    liftIO . forkIO $ mapM_ (\i -> do
        writeChan chan $ ServerEvent Nothing Nothing [fromShow i]
        threadDelay (10^6)) [0::Int ..]

    run 3000 . runApiary def $ do
        [capture|es|] $ action (eventSourceChan chan)
        root          $ action (file "eventsource.html" Nothing)

