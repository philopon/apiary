{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Web.Apiary.EventSource 
    ( eventSourceIO, eventSourceChan
    , module Network.Wai.EventSource.EventStream
    ) where

import Web.Apiary
import Network.Wai.EventSource.EventStream (ServerEvent(..))
import qualified Network.Wai.EventSource.EventStream as E

import Control.Concurrent.Chan (Chan, dupChan, readChan)

import Data.Function

#ifdef VERSION_wai_eventsource
import           Blaze.ByteString.Builder (Builder)
import           Data.Conduit

ioToSource :: IO ServerEvent -> Source IO (Flush Builder)
ioToSource a =
    loop
  where
    loop = do
        x <- liftIO a
        case E.eventToBuilder x of
            Nothing -> return ()
            Just y -> do
                yield $ Chunk y
                yield Flush
                loop
#else
ioToSource :: IO ServerEvent -> StreamingBody
ioToSource src send flush = fix $ \loop -> do
    se <- src
    case E.eventToBuilder se of
        Nothing -> return ()
        Just b  -> send b >> flush >> loop
#endif

-- | eventsource with io action. since 0.11.3.
eventSourceIO :: Monad m => IO ServerEvent -> ActionT m ()
eventSourceIO io = do
    status status200
    contentType "text/event-stream"
    stream $ ioToSource io

-- | eventsource with chan. since 0.11.3.
eventSourceChan :: MonadIO m => Chan ServerEvent -> ActionT m ()
eventSourceChan chan = do
    chan' <- liftIO $ dupChan chan
    eventSourceIO (readChan chan')
