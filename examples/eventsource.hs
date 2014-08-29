{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Web.Apiary
import Web.Apiary.EventSource
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder.Char8
import Control.Concurrent
import Language.Haskell.TH
import System.FilePath
import System.Directory

main :: IO ()
main = do
    setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)
    chan <- liftIO newChan
    _ <- liftIO . forkIO $ mapM_ (\i -> do
        writeChan chan $ ServerEvent Nothing Nothing [fromShow i]
        threadDelay (10^(6:: Int))) [0::Int ..]

    server (run 3000) . runApiary def $ do
        [capture|es|] $ action (eventSourceChan chan)
        root          $ action (file "eventsource.html" Nothing)

