{-# LANGUAGE OverloadedStrings #-}

module Web.Apiary.EventSource 
    ( eventSourceIO, eventSourceChan
    , module Network.Wai.EventSource.EventStream
    ) where

import Web.Apiary(MonadIO(..), status200)
import Network.Wai.EventSource.EventStream (ServerEvent(..))
import qualified Network.Wai.EventSource.EventStream as E

import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Monad.Apiary.Action
    (ActionT, status, contentType, stream, StreamingBody)

import Data.Function(fix)

ioToSource :: IO ServerEvent -> StreamingBody
ioToSource src send flush = fix $ \loop -> do
    se <- src
    case E.eventToBuilder se of
        Nothing -> return ()
        Just b  -> send b >> flush >> loop

-- | eventsource with io action. since 0.11.3.
eventSourceIO :: Monad m => IO ServerEvent -> ActionT exts prms m ()
eventSourceIO io = do
    status status200
    contentType "text/event-stream"
    stream $ ioToSource io

-- | eventsource with chan. since 0.11.3.
eventSourceChan :: MonadIO m => Chan ServerEvent -> ActionT exts prms m ()
eventSourceChan chan = do
    chan' <- liftIO $ dupChan chan
    eventSourceIO (readChan chan')
