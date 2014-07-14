{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Apiary.Logger.Internal where

import Web.Apiary hiding (Default(..), File)
import System.Log.FastLogger
import Control.Exception
import Data.Default.Class

data LogDest
    = File FilePath
    | Stdout
    | Stderr
    | Null

data LogConfig = LogConfig
    { bufferSize :: BufSize
    , logDest    :: LogDest
    }

data Logger = Logger
    { pushLog  :: LogStr -> IO ()
    , closeLog :: IO ()
    }

instance Default LogConfig where
    def = LogConfig defaultBufSize Stderr

newLogger :: BufSize -> LogDest -> IO Logger
newLogger s (File p) = newFileLoggerSet s p >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger s Stdout = newStdoutLoggerSet s >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger s Stderr = newStderrLoggerSet s >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger _ Null = return $ Logger (\_ -> return ()) (return ())

withLogger :: LogConfig -> (Logger -> IO a) -> IO a
withLogger LogConfig{..} m = bracket
    (newLogger bufferSize logDest)
    closeLog
    (\l -> m l)

logging :: MonadIO m => Logger -> LogStr -> ActionT m ()
logging logger msg = liftIO $ pushLog logger msg

