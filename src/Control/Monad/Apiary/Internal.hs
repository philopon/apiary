{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Apiary.Internal where

import Network.Wai
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Apiary.SList

import Control.Monad.Apiary.Action.Internal

newtype ApiaryT c m a = ApiaryT { unApiaryT :: forall b.
    (forall x . m x -> IO x)
    -> (Request -> Maybe (SList c))
    -> ApiaryConfig
    -> (a -> ActionT IO () -> m b)
    -> m b 
    }

filterToActionT :: Monad m => (Request -> Maybe (SList a))
                -> ActionT m (SList a)
filterToActionT f = getRequest >>= maybe mzero return . f

instance Functor (ApiaryT c m) where
    fmap f m = ApiaryT $ \run grd conf cont ->
        unApiaryT m run grd conf $ \a hdr -> hdr `seq` cont (f a) hdr

instance Applicative (ApiaryT c m) where
    pure x = ApiaryT $ \_ _ _ cont -> cont x mempty
    mf <*> ma = ApiaryT $ \run grd conf cont ->
        unApiaryT mf run grd conf $ \f hdr  ->
        unApiaryT ma run grd conf $ \a hdr' ->
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont (f a) hdr''

instance Monad (ApiaryT c m) where
    return x = ApiaryT $ \_ _ _ cont -> cont x mempty
    m >>= k = ApiaryT $ \run grd conf cont ->
        unApiaryT m run grd conf $ \a hdr ->
        unApiaryT (k a) run grd conf $ \b hdr' -> 
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont b hdr''

runApiaryT :: Monad m => ApiaryConfig -> (forall x. m x -> IO x) -> ApiaryT '[] m a -> Application
runApiaryT conf run m req = run (unApiaryT m run (\_ -> Just SNil) conf (\_ w -> return w)) >>= \a ->
    execActionT conf a req

type Apiary c = ApiaryT c IO

runApiary :: ApiaryConfig -> Apiary '[] a -> Application
runApiary conf = runApiaryT conf id

getRunner :: Monad m => ApiaryT c m (ActionT m a -> ActionT IO a)
getRunner = ApiaryT $ \run _ _ c -> c (hoistActionT run) mempty

getGuard :: ApiaryT c m (Request -> Maybe (SList c))
getGuard = ApiaryT $ \_ grd _ c -> c grd mempty

apiaryConfig :: ApiaryT c m ApiaryConfig
apiaryConfig = ApiaryT $ \_ _ c cont -> cont c mempty

addRoute :: ActionT IO () -> ApiaryT c m ()
addRoute r = ApiaryT $ \_ _ _ cont -> cont () r

focus :: Monad m => (Request -> SList c -> Maybe (SList c')) -> ApiaryT c' m b -> ApiaryT c m b
focus g m = do
    ApiaryT $ \run grd cfg cont ->
        unApiaryT m run (\r -> grd r >>= \c -> g r c) cfg cont

action :: Monad m => Fn c (ActionT m ()) -> ApiaryT c m ()
action = actionWithPreAction (const $ return ())

-- | execute action before main action. since v0.4.2.0
actionWithPreAction :: Monad m => (SList xs -> ActionT IO a)
                    -> Fn xs (ActionT m ()) -> ApiaryT xs m ()
actionWithPreAction pa a = do
    tr  <- getRunner
    grd <- getGuard
    addRoute $ filterToActionT grd >>= \c -> (pa c) >> tr (apply a c)
