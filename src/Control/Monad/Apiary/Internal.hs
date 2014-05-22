{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Apiary.Internal where

import Network.Wai
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Control
import Control.Monad.Base
import Data.Apiary.SList

import Control.Monad.Apiary.Action.Internal

newtype ApiaryT c m a = ApiaryT { unApiaryT :: forall b.
    Action (SList c)
    -> ApiaryConfig
    -> (a -> Action () -> m b)
    -> m b 
    }

instance Functor (ApiaryT m c) where
    fmap f m = ApiaryT $ \grd conf cont ->
        unApiaryT m grd conf $ \a hdr -> hdr `seq` cont (f a) hdr

instance Applicative (ApiaryT m c) where
    pure x = ApiaryT $ \_ _ cont -> cont x empty
    mf <*> ma = ApiaryT $ \grd conf cont ->
        unApiaryT mf grd conf $ \f hdr  ->
        unApiaryT ma grd conf $ \a hdr' ->
        let hdr'' = hdr <|> hdr'
        in hdr'' `seq` cont (f a) hdr''

instance Monad (ApiaryT m c) where
    return x = ApiaryT $ \_ _ cont -> cont x empty
    m >>= k = ApiaryT $ \grd conf cont ->
        unApiaryT    m  grd conf $ \a hdr  ->
        unApiaryT (k a) grd conf $ \b hdr' -> 
        let hdr'' = hdr <|> hdr'
        in hdr'' `seq` cont b hdr''

instance MonadTrans (ApiaryT c) where
    lift m = ApiaryT $ \_ _ c -> m >>= \a -> c a empty

instance MonadIO m => MonadIO (ApiaryT c m) where
    liftIO m = ApiaryT $ \_ _ c -> liftIO m >>= \a -> c a empty

instance MonadBase b m => MonadBase b (ApiaryT c m) where
    liftBase m = ApiaryT $ \_ _ c -> liftBase m >>= \a -> c a empty

apiaryT :: Monad m
        => (Action (SList c) -> ApiaryConfig -> m (a,Action ()))
        -> ApiaryT c m a
apiaryT f = ApiaryT $ \grd conf cont -> f grd conf >>= \(a,w) -> cont a w


instance MonadTransControl (ApiaryT c) where
    newtype StT (ApiaryT c) a = StTApiary { unStTApiary :: (a, Action ()) }
    liftWith f = apiaryT $ \g c ->
        liftM (\a -> (a, empty)) 
        (f $ \t -> liftM StTApiary $ unApiaryT t g c (\a w -> return (a,w)))
    restoreT m = apiaryT $ \_ _ -> liftM unStTApiary m

instance MonadBaseControl b m => MonadBaseControl b (ApiaryT c m) where
    newtype StM (ApiaryT c m) a = StMApiary { unStMApiary :: ComposeSt (ApiaryT c) m a }
    liftBaseWith = defaultLiftBaseWith StMApiary
    restoreM     = defaultRestoreM   unStMApiary

type Apiary c = ApiaryT c Identity

runApiary :: ApiaryConfig -> Apiary '[] a -> Application
runApiary conf = runIdentity . runApiaryT conf

runApiaryT :: Monad m => ApiaryConfig -> ApiaryT '[] m a -> m Application
runApiaryT conf m = unApiaryT m (return SNil) conf (\_ w -> return w) >>=
    return . execAction conf

getGuard :: ApiaryT c m (Action (SList c))
getGuard = ApiaryT $ \grd _ c -> c grd empty

apiaryConfig :: ApiaryT c m ApiaryConfig
apiaryConfig = ApiaryT $ \_ c cont -> cont c empty

addRoute :: Action () -> ApiaryT c m ()
addRoute r = ApiaryT $ \_ _ cont -> cont () r

-- | filter by action. since 0.6.1.0.
focus :: (SList c -> Action (SList c')) -> ApiaryT c' m a -> ApiaryT c m a
focus g m = do
    ApiaryT $ \grd cfg cont -> unApiaryT m (grd >>= g) cfg cont

action :: Fn c (Action ()) -> ApiaryT c m ()
action = actionWithPreAction (const $ return ())

-- | execute action before main action. since v0.4.2.0
actionWithPreAction :: (SList xs -> Action a)
                    -> Fn xs (Action ()) -> ApiaryT xs m ()
actionWithPreAction pa a = do
    grd <- getGuard
    addRoute $ grd >>= \c -> (pa c) >> apply a c
