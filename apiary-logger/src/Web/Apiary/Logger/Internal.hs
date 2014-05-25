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

import Control.Applicative
import Web.Apiary hiding (Default(..))
import System.Log.FastLogger
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Exception
import Data.Reflection
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

type HasLogger = Given Logger

newLogger :: BufSize -> LogDest -> IO Logger
newLogger s (File p) = newFileLoggerSet s p >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger s Stdout = newStdoutLoggerSet s >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger s Stderr = newStderrLoggerSet s >>= \l -> 
    return $ Logger (pushLogStr l) (flushLogStr l)
newLogger _ Null = return $ Logger (\_ -> return ()) (return ())

withLogger :: LogConfig -> (HasLogger => IO a) -> IO a
withLogger LogConfig{..} m = bracket
    (newLogger bufferSize logDest)
    closeLog
    (\l -> give l m)

withLogger' :: LogConfig
            -> ((forall r. (HasLogger => r) -> r) -> IO a) -> IO a
withLogger' LogConfig{..} m = bracket
    (newLogger bufferSize logDest)
    closeLog
    (\l -> m (give l))

logging :: (MonadIO m, HasLogger) => LogStr -> ActionT m ()
logging msg = liftIO $ pushLog given msg

newtype GivenLoggerT m a = GivenLoggerT { runGivenLoggerT :: m a }
    deriving(Functor, Applicative, Monad, MonadIO)

instance MonadBase b m => MonadBase b (GivenLoggerT m) where
    liftBase = GivenLoggerT . liftBase

instance MonadTrans GivenLoggerT where
    lift = GivenLoggerT

instance MonadBaseControl b m => MonadBaseControl b (GivenLoggerT m) where
    newtype StM (GivenLoggerT m) a = StMGivenLogger { unStMGivenLogger :: ComposeSt GivenLoggerT m a }
    liftBaseWith = defaultLiftBaseWith StMGivenLogger
    restoreM     = defaultRestoreM   unStMGivenLogger

instance MonadTransControl GivenLoggerT where
    newtype StT GivenLoggerT a = StGivenLogger { unStGivenLogger :: a }
    liftWith f = GivenLoggerT $ f $ liftM StGivenLogger . runGivenLoggerT
    restoreT   = GivenLoggerT . liftM unStGivenLogger

instance (MonadIO m, HasLogger) => MonadLogger (GivenLoggerT m) where
    monadLoggerLog loc src lv msg = GivenLoggerT . liftIO $
        pushLog given (defaultLogStr loc src lv (toLogStr msg))

instance (MonadIO m, HasLogger) => MonadLogger (ActionT m) where
    monadLoggerLog loc src lv msg =
        logging $ defaultLogStr loc src lv (toLogStr msg)
