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

-- | most generic Apiary monad. since 0.8.0.0.
newtype ApiaryT c n m a = ApiaryT { unApiaryT :: forall b.
    ActionT n (SList c)
    -> ApiaryConfig
    -> (a -> ActionT n () -> m b)
    -> m b 
    }

-- | no transformer. (ActionT IO, ApiaryT Identity)
type Apiary c = ApiaryT c IO Identity

instance Functor (ApiaryT c n m) where
    fmap f m = ApiaryT $ \grd conf cont ->
        unApiaryT m grd conf $ \a hdr -> hdr `seq` cont (f a) hdr

instance (Monad n, Functor n) => Applicative (ApiaryT c n m) where
    pure x = ApiaryT $ \_ _ cont -> cont x empty
    mf <*> ma = ApiaryT $ \grd conf cont ->
        unApiaryT mf grd conf $ \f hdr  ->
        unApiaryT ma grd conf $ \a hdr' ->
        let hdr'' = hdr <|> hdr'
        in hdr'' `seq` cont (f a) hdr''

instance (Monad n, Functor n) => Monad (ApiaryT c n m) where
    return x = ApiaryT $ \_ _ cont -> cont x empty
    m >>= k = ApiaryT $ \grd conf cont ->
        unApiaryT    m  grd conf $ \a hdr  ->
        unApiaryT (k a) grd conf $ \b hdr' -> 
        let hdr'' = hdr <|> hdr'
        in hdr'' `seq` cont b hdr''

instance (Functor n, Monad n) => MonadTrans (ApiaryT c n) where
    lift m = ApiaryT $ \_ _ c -> m >>= \a -> c a empty

instance (Functor n, Monad n, MonadIO m) => MonadIO (ApiaryT c n m) where
    liftIO m = ApiaryT $ \_ _ c -> liftIO m >>= \a -> c a empty

instance (Functor n, Monad n, MonadBase b m) => MonadBase b (ApiaryT c n m) where
    liftBase m = ApiaryT $ \_ _ c -> liftBase m >>= \a -> c a empty

apiaryT :: Monad m
        => (ActionT n (SList c) -> ApiaryConfig -> m (a, ActionT n ()))
        -> ApiaryT c n m a
apiaryT f = ApiaryT $ \grd conf cont -> f grd conf >>= \(a,w) -> cont a w

instance (Functor n, Monad n) => MonadTransControl (ApiaryT c n) where
    newtype StT (ApiaryT c n) a = StTApiary' { unStTApiary' :: (a, ActionT n ()) }
    liftWith f = apiaryT $ \g c ->
        liftM (\a -> (a, empty)) 
        (f $ \t -> liftM StTApiary' $ unApiaryT t g c (\a w -> return (a,w)))
    restoreT m = apiaryT $ \_ _ -> liftM unStTApiary' m

instance (Functor n, Monad n, MonadBaseControl b m) => MonadBaseControl b (ApiaryT c n m) where
    newtype StM (ApiaryT c n m) a = StMApiary' { unStMApiary' :: ComposeSt (ApiaryT c n) m a }
    liftBaseWith = defaultLiftBaseWith StMApiary'
    restoreM     = defaultRestoreM   unStMApiary'

runApiaryT :: (Monad n, Monad m) => (forall b. n b -> IO b) -> ApiaryConfig
            -> ApiaryT '[] n m a -> m Application
runApiaryT run conf m = unApiaryT m (return SNil) conf (\_ w -> return w) >>= \act ->
    return $ execActionT conf (hoistActionT run act)

runApiary :: ApiaryConfig -> Apiary '[] a -> Application
runApiary conf m = runIdentity $ runApiaryT id conf m

class MonadApiary c' m where
  foa :: (SList c -> ActionT n (SList c')) -> m a -> m a

getGuard :: (Functor n, Monad n) => ApiaryT c n m (ActionT n (SList c))
getGuard = ApiaryT $ \grd _ c -> c grd empty

apiaryConfig :: (Functor n, Monad n) => ApiaryT c n m ApiaryConfig
apiaryConfig = ApiaryT $ \_ c cont -> cont c empty

addRoute :: (Functor n, Monad n) => ActionT n () -> ApiaryT c n m ()
addRoute r = ApiaryT $ \_ _ cont -> cont () r

-- | filter by action. since 0.6.1.0.
focus :: (Functor n, Monad n) => (SList c -> ActionT n (SList c'))
      -> ApiaryT c' n m a -> ApiaryT c n m a
focus g m = ApiaryT $ \grd cfg cont -> unApiaryT m (grd >>= g) cfg cont

-- | splice ActionT ApiaryT.
action :: (Functor n, Monad n) => Fn c (ActionT n ()) -> ApiaryT c n m ()
action a = action' $ apply a

{-# DEPRECATED actionWithPreAction "use action'" #-}
-- | execute action before main action. since v0.4.2.0
actionWithPreAction :: (Functor n, Monad n) => (SList xs -> ActionT n a)
                    -> Fn xs (ActionT n ()) -> ApiaryT xs n m ()
actionWithPreAction pa a = do
    action' $ \c -> pa c >> apply a c

-- | like action. but not apply arguments. since 0.8.0.0.
action' :: (Functor n, Monad n) => (SList c -> ActionT n ()) -> ApiaryT c n m ()
action' a = do
    grd <- getGuard
    addRoute $ grd >>= \c -> a c
