{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Web.Apiary.Trans where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Network.Wai
import Data.Default
import Data.Monoid
import Network.HTTP.Types.Status

data ApiaryConfig m = ApiaryConfig
    { notFound :: ApplicationM m }

instance Monad m => Default (ApiaryConfig m) where
    def = ApiaryConfig 
        { notFound = \_ -> return $ responseLBS status404 
            [("Content-Type", "text/plain")] "404 Page Notfound."
        }

type ApplicationM m = Request -> m Response

newtype ActionT m a = ActionT
    { unActionT :: ReaderT (ApiaryConfig m) (ReaderT Request (MaybeT m)) a 
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans ActionT where
    lift = ActionT . lift . lift . lift

runActionT :: Monad m => ApiaryConfig m -> ActionT m Response -> (ApplicationM m)
runActionT config (ActionT m) request =
    runMaybeT (runReaderT (runReaderT m config) request) >>= \case
        Nothing -> notFound config request
        Just r  -> return r

instance (Monad m, Functor m) => Alternative (ActionT m) where
    empty = mzero
    (<|>) = mplus

instance Monad m => MonadPlus (ActionT m) where
    mzero = ActionT . ReaderT $ \_ -> ReaderT $ \_ -> MaybeT (return Nothing)
    mplus (ActionT m) (ActionT n) = ActionT . ReaderT $ \c -> ReaderT $ \r ->
        MaybeT $ runMaybeT (runReaderT (runReaderT m c) r) >>= \case
            Just a  -> return $ Just a
            Nothing -> runMaybeT (runReaderT (runReaderT n c) r)

instance Monad m => Monoid (ActionT m Response) where
    mempty  = mzero
    mappend = mplus

getRequest :: Monad m => ActionT m Request
getRequest = ActionT .lift $ ask

newtype ApiaryT m a = ApiaryM { unApiaryT :: Writer (ActionT m Response) a }
    deriving (Functor, Applicative, Monad)

runApiaryT :: Monad m => ApiaryConfig m -> ApiaryT m a -> ApplicationM m
runApiaryT config (ApiaryM m) = runActionT config (execWriter m)

addRoute :: Monad m => ActionT m Response -> ApiaryT m ()
addRoute = ApiaryM . tell

function :: Monad m => (Request -> Bool) -> ActionT m Response -> ApiaryT m ()
function f a = addRoute $ getRequest >>= \r -> if f r then a else mempty

