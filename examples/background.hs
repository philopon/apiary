{-# LANGUAGE OverloadedStrings, QuasiQuotes, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import Web.Apiary
import Web.Apiary.Heroku
import Network.Wai.Handler.Warp
import qualified Web.Apiary.MongoDB as M
import Data.Time
import Control.Concurrent.Lifted

main :: IO ()
main = runHerokuWith run (M.initHerokuMongoDB def) def $ do
    _ <- fork worker
    action $ do
        contentType "text/plain"
        time <- liftIO getCurrentTime
        log_ <- M.access $ do
            M.insert_ "log" ["type" M.=: ("webrq" :: String),  "time" M.=: time]
            M.find (M.select [] "log") >>= M.rest
        mapM_ (\l -> showing l >> char '\n') log_

    [capture|/flush|] . action $ M.access $ M.delete (M.select [] "log")

worker :: (Has M.MongoDB exts, Has Heroku exts) => ApiaryT exts prms IO IO ()
worker = do
    time <- liftIO getCurrentTime
    M.access $
        M.insert_ "log" ["type" M.=: ("timer" :: String),  "time" M.=: time]
    threadDelay (600 * 10^(6::Int))
    worker
