{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Apiary.Internal where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader

import Control.Monad.Apiary.Action.Internal

newtype ApiaryT c m a = ApiaryT { unApiaryT :: ReaderT (ActionT m c) (ReaderT (ApiaryConfig m) (Writer (ActionT m ()))) a }
    deriving (Functor, Applicative, Monad)

runApiaryT' :: Monad m => ApiaryConfig m -> ApiaryT c m a -> ActionT m c -> ApplicationM m
runApiaryT' config (ApiaryT m) = runActionT config . execWriter . flip runReaderT config . runReaderT m

runApiaryT :: Monad m => ApiaryConfig m -> ApiaryT () m a -> ApplicationM m
runApiaryT conf m = runApiaryT' conf m $ return ()

apiaryConfig :: Monad m => ApiaryT c m (ApiaryConfig m)
apiaryConfig = ApiaryT $ lift ask

focus :: Monad m => (c -> ActionT m c') -> ApiaryT c' m a -> ApiaryT c m a
focus f (ApiaryT m) = ApiaryT . ReaderT $ \c -> runReaderT m (c >>= f)

action_ :: Monad m => ActionT m () -> ApiaryT c m ()
action_ a = action (const a)

action :: Monad m => (c -> ActionT m ()) -> ApiaryT c m ()
action a = ApiaryT $ ask >>= \g -> (lift . lift) (tell $ g >>= a)

