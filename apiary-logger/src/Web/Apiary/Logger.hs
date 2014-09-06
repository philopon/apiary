{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Apiary.Logger
    ( Logger
    -- * configuration
    , LogDest(..), LogConfig(..)
    -- * initialize
    , initLogger
    -- * action
    , logging
    -- * wrapper
    , LogWrapper, logWrapper, runLogWrapper
    ) where

import System.Log.FastLogger

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Exception.Lifted

import Data.Default.Class

import Data.Apiary.Proxy
import Web.Apiary
import Data.Apiary.Extension

data LogDest
    = LogFile FilePath
    | LogStdout
    | LogStderr
    | NoLog

data LogConfig = LogConfig
    { bufferSize :: BufSize
    , logDest    :: LogDest
    }

instance Default LogConfig where
    def = LogConfig defaultBufSize LogStderr

-- | logger extension data type.
data Logger = Logger
    { pushLog  :: LogStr -> IO ()
    , closeLog :: IO ()
    }

newLogger :: BufSize -> LogDest -> IO Logger
newLogger s (LogFile p) = newFileLoggerSet s p >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger s LogStdout = newStdoutLoggerSet s >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger s LogStderr = newStderrLoggerSet s >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger _ NoLog = return $ Logger (\_ -> return ()) (return ())

-- | logger initializer.
initLogger :: (MonadBaseControl IO m, MonadIO m) => LogConfig -> Initializer' m Logger
initLogger LogConfig{..} = initializerBracket $ bracket
    (liftIO $ newLogger bufferSize logDest)
    (liftIO . closeLog)

-- | push log.
logging :: (Has Logger exts, MonadIO m)
        => LogStr -> ActionT exts m ()
logging m = do
    l <- getExt (Proxy :: Proxy Logger)
    liftIO $ pushLog l m

instance (MonadIO m, Has Logger exts) => MonadLogger (ActionT exts m) where
    monadLoggerLog loc src lv msg = do
        l <- getExt (Proxy :: Proxy Logger)
        liftIO . pushLog l $ defaultLogStr loc src lv (toLogStr msg)

instance (Monad actM, MonadIO m, Has Logger exts) => MonadLogger (ApiaryT exts prms actM m) where
    monadLoggerLog loc src lv msg = do
        l <- apiaryExt (Proxy :: Proxy Logger)
        liftIO . pushLog l $ defaultLogStr loc src lv (toLogStr msg)

-- | wrapper to use as MonadLogger using Logger Extenson.
newtype LogWrapper exts m a =
    LogWrapper { unLogWrapper :: ReaderT (Extensions exts) m a }
    deriving ( Functor, Applicative
             , Monad, MonadIO, MonadTrans, MonadBase b)

logWrapper :: Monad m => m a -> LogWrapper exts m a
logWrapper = LogWrapper . lift

runLogWrapper :: Extensions exts -> LogWrapper exts m a -> m a
runLogWrapper e = flip runReaderT e . unLogWrapper

instance (MonadIO m, Has Logger exts) => MonadLogger (LogWrapper exts m) where
    monadLoggerLog loc src lv msg = do
        l <- getExtension (Proxy :: Proxy Logger) `liftM` LogWrapper ask
        liftIO . pushLog l $ defaultLogStr loc src lv (toLogStr msg)

instance MonadTransControl (LogWrapper exts) where
    newtype StT (LogWrapper exts) a = StLogWrapper { unStLogWrapper :: StT (ReaderT (Extensions exts)) a }
    liftWith = defaultLiftWith LogWrapper unLogWrapper StLogWrapper
    restoreT = defaultRestoreT LogWrapper unStLogWrapper

instance MonadBaseControl b m => MonadBaseControl b (LogWrapper exts m) where
    newtype StM (LogWrapper exts m) a = StMLogWrapper { unStMLogWrapper :: ComposeSt (LogWrapper exts) m a }
    liftBaseWith = defaultLiftBaseWith StMLogWrapper
    restoreM     = defaultRestoreM     unStMLogWrapper
