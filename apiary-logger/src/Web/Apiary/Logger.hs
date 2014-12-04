{-# LANGUAGE RecordWildCards #-}
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

import qualified System.Log.FastLogger as FL

import Control.Applicative(Applicative)
import Control.Monad.Base(MonadBase)
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.Logger(MonadLogger(..), defaultLogStr)
import Control.Monad.Trans.Reader(ReaderT(..), ask)
import Control.Monad.Trans.Control
    ( MonadTransControl(..), MonadBaseControl(..)
    , defaultLiftWith, defaultRestoreT
    , ComposeSt, defaultLiftBaseWith, defaultRestoreM
    )
import Control.Exception.Lifted(bracket)

import Data.Default.Class(Default(..))

import Data.Apiary.Compat(Proxy(..))
import Data.Apiary.Extension
     ( Has, Initializer', initializerBracket'
     , Extensions, Extension, MonadExts(getExts), getExt
     )

data LogDest
    = LogFile FilePath
    | LogStdout
    | LogStderr
    | NoLog

data LogConfig = LogConfig
    { bufferSize :: FL.BufSize
    , logDest    :: LogDest
    }

instance Default LogConfig where
    def = LogConfig FL.defaultBufSize LogStderr

-- | logger extension data type.
data Logger = Logger
    { pushLog  :: FL.LogStr -> IO ()
    , closeLog :: IO ()
    }
instance Extension Logger

newLogger :: FL.BufSize -> LogDest -> IO Logger
newLogger s (LogFile p) = FL.newFileLoggerSet s p >>= \l -> 
    return $ Logger (FL.pushLogStr l) (FL.flushLogStr l)
newLogger s LogStdout = FL.newStdoutLoggerSet s >>= \l -> 
    return $ Logger (FL.pushLogStr l) (FL.flushLogStr l)
newLogger s LogStderr = FL.newStderrLoggerSet s >>= \l -> 
    return $ Logger (FL.pushLogStr l) (FL.flushLogStr l)
newLogger _ NoLog = return $ Logger (\_ -> return ()) (return ())

-- | logger initializer.
initLogger :: (MonadBaseControl IO m, MonadIO m) => LogConfig -> Initializer' m Logger
initLogger LogConfig{..} = initializerBracket' $ bracket
    (liftIO $ newLogger bufferSize logDest)
    (liftIO . closeLog)

-- | push log.
logging :: (Has Logger es, MonadExts es m, MonadIO m) => FL.LogStr -> m ()
logging m = getExt (Proxy :: Proxy Logger) >>= \l -> liftIO $ pushLog l m

instance (Has Logger es, MonadExts es m, MonadIO m) => MonadLogger m where
    monadLoggerLog loc src lv msg = logging $ defaultLogStr loc src lv (FL.toLogStr msg)

-- | wrapper to use as MonadLogger using Logger Extenson.
newtype LogWrapper exts m a =
    LogWrapper { unLogWrapper :: ReaderT (Extensions exts) m a }
    deriving ( Functor, Applicative
             , Monad, MonadIO, MonadTrans, MonadBase b)

logWrapper :: Monad m => m a -> LogWrapper exts m a
logWrapper = LogWrapper . lift

runLogWrapper :: Extensions exts -> LogWrapper exts m a -> m a
runLogWrapper e = flip runReaderT e . unLogWrapper

instance MonadTransControl (LogWrapper exts) where
    newtype StT (LogWrapper exts) a = StLogWrapper { unStLogWrapper :: StT (ReaderT (Extensions exts)) a }
    liftWith = defaultLiftWith LogWrapper unLogWrapper StLogWrapper
    restoreT = defaultRestoreT LogWrapper unStLogWrapper

instance MonadBaseControl b m => MonadBaseControl b (LogWrapper exts m) where
    newtype StM (LogWrapper exts m) a = StMLogWrapper { unStMLogWrapper :: ComposeSt (LogWrapper exts) m a }
    liftBaseWith = defaultLiftBaseWith StMLogWrapper
    restoreM     = defaultRestoreM     unStMLogWrapper

instance Monad m => MonadExts exts (LogWrapper exts m) where
    getExts = LogWrapper ask
