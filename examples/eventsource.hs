{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Web.Apiary
import Web.Apiary.EventSource
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder.Char8
import qualified Data.Text as T
import Control.Concurrent
import Language.Haskell.TH
import System.FilePath
import System.Directory

main :: IO ()
main = do
    setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)
    chan <- liftIO newChan
    liftIO . forkIO $ mapM_ (\i -> do
        writeChan chan $ ServerEvent Nothing Nothing [fromShow i]
        threadDelay (10^6)) [0::Int ..]

    run 3000 . runApiary def $ do
        [capture|es|] $ action (eventSourceChan chan)
        root          $ action (file "eventsource.html" Nothing)

