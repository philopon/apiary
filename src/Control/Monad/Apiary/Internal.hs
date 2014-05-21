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
import Control.Monad.Trans.Control
import Control.Monad.Base
import Data.Monoid
import Data.Apiary.SList

import Control.Monad.Apiary.Action.Internal

newtype Apiary c a = Apiary { unApiary :: forall b.
    Action (SList c)
    -> ApiaryConfig
    -> (a -> Action () -> IO b)
    -> IO b 
    }

instance Functor (Apiary c) where
    fmap f m = Apiary $ \grd conf cont ->
        unApiary m grd conf $ \a hdr -> hdr `seq` cont (f a) hdr

instance Applicative (Apiary c) where
    pure x = Apiary $ \_ _ cont -> cont x mempty
    mf <*> ma = Apiary $ \grd conf cont ->
        unApiary mf grd conf $ \f hdr  ->
        unApiary ma grd conf $ \a hdr' ->
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont (f a) hdr''

instance Monad (Apiary c) where
    return x = Apiary $ \_ _ cont -> cont x mempty
    m >>= k = Apiary $ \grd conf cont ->
        unApiary    m  grd conf $ \a hdr  ->
        unApiary (k a) grd conf $ \b hdr' -> 
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont b hdr''

instance MonadIO (Apiary c) where
    liftIO m = Apiary $ \_ _ c -> m >>= \a -> c a mempty

instance MonadBase IO (Apiary c) where
    liftBase = liftIO

apiary :: (Action (SList c) -> ApiaryConfig -> IO (a,Action ())) -> Apiary c a
apiary f = Apiary $ \grd conf cont -> f grd conf >>= \(a,w) -> cont a w

run :: Apiary c a -> Action (SList c) -> ApiaryConfig -> IO (a, Action ())
run m grd conf = unApiary m grd conf $ \a w -> return (a,w)

instance MonadBaseControl IO (Apiary c) where
    newtype StM (Apiary c) a = StMApiary { unStMApiary :: (a, Action ()) }
    liftBaseWith f = apiary $ \g c ->
        liftM (\a -> (a, mempty)) (f $ \t -> liftM StMApiary $ run t g c)
    restoreM m = apiary $ \_ _ -> return (unStMApiary m)

runApiary :: ApiaryConfig -> Apiary '[] a -> Application
runApiary conf m req = unApiary m (return SNil) conf (\_ w -> return w) >>= \a ->
    execAction conf a req

getGuard :: Apiary c (Action (SList c))
getGuard = Apiary $ \grd _ c -> c grd mempty

apiaryConfig :: Apiary c ApiaryConfig
apiaryConfig = Apiary $ \_ c cont -> cont c mempty

addRoute :: Action () -> Apiary c ()
addRoute r = Apiary $ \_ _ cont -> cont () r

-- | filter by action. since 0.6.1.0.
focus :: (SList c -> Action (SList c')) -> Apiary c' a -> Apiary c a
focus g m = do
    Apiary $ \grd cfg cont ->
        unApiary m (grd >>= g) cfg cont

action :: Fn c (Action ()) -> Apiary c ()
action = actionWithPreAction (const $ return ())

-- | execute action before main action. since v0.4.2.0
actionWithPreAction :: (SList xs -> Action a)
                    -> Fn xs (Action ()) -> Apiary xs ()
actionWithPreAction pa a = do
    grd <- getGuard
    addRoute $ grd >>= \c -> (pa c) >> apply a c
