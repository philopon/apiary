{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Apiary.Logger (
    -- * configuration
    I.LogDest(..), I.LogConfig(..), HasLogger
    -- * initialize
    , withLogger
    , withLogger'
    -- * action
    , logging
    -- * wrapper
    , GivenLoggerT(..)

    -- * reexports
    , module Data.Default.Class
    ) where

import qualified Web.Apiary.Logger.Internal as I
import Web.Apiary

import System.Log.FastLogger

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Logger

import Data.Default.Class
import Data.Reflection

type HasLogger = Given I.Logger

withLogger :: I.LogConfig -> (HasLogger => IO a) -> IO a
withLogger c m = I.withLogger c (\l -> give l m)

withLogger' :: I.LogConfig -> (((HasLogger => r) -> r) -> IO a) -> IO a
withLogger' c m = I.withLogger c (\l -> m (give l))

logging :: (MonadIO m, HasLogger) => LogStr -> ActionT m ()
logging = I.logging given

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
        I.pushLog given (defaultLogStr loc src lv (toLogStr msg))

instance (MonadIO m, HasLogger) => MonadLogger (ActionT m) where
    monadLoggerLog loc src lv msg =
        I.logging given $ defaultLogStr loc src lv (toLogStr msg)
