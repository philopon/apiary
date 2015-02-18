{-# LANGUAGE OverloadedStrings, QuasiQuotes, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, ExistentialQuantification, GeneralizedNewtypeDeriving #-}

import Web.Apiary
import Network.Wai.Handler.Warp
import Control.Concurrent.Lifted

main :: IO ()
main = runApiary (run 3000) def $ do
    mv <- liftIO $ newMVar (0 :: Int)
    _ <- fork $ liftIO $ worker mv
    action $ do
        contentType "text/plain"
        i <- readMVar mv
        showing i

worker :: MVar Int -> IO ()
worker mv = go
  where
    go = do
        modifyMVar_ mv (return . succ)
        threadDelay (10 ^ (6::Int))
        go
