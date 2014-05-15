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
import Data.Monoid

import Control.Monad.Apiary.Action.Internal

newtype ApiaryT c m a = ApiaryT { unApiaryT :: forall b.
    (forall x . m x -> IO x)
    -> ActionT IO (SList c)
    -> ApiaryConfig
    -> (a -> ActionT IO () -> m b)
    -> m b 
    }

data SList (as :: [*]) where
    SNil  :: SList '[]
    SCons :: a -> SList xs -> SList (a ': xs)

type family Fn (as :: [*]) r
type instance Fn '[] r = r
type instance Fn (x ': xs) r = x -> Fn xs r

type family Snoc (as :: [*]) a :: [*]
type instance Snoc '[] a = '[a]
type instance Snoc (x ': xs) a = x ': Snoc xs a

apply :: Fn xs r -> SList xs -> r
apply v SNil = v
apply f (SCons a as) = apply (f a) as

sSnoc :: SList as -> a -> SList (Snoc as a)
sSnoc SNil         a = SCons a SNil
sSnoc (SCons x xs) a = SCons x $ sSnoc xs a

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
runApiaryT conf run m req = run (unApiaryT m run (return SNil) conf (\_ w -> return w)) >>= \a ->
    execActionT conf a req

type Apiary c = ApiaryT c IO

runApiary :: ApiaryConfig -> Apiary '[] a -> Application
runApiary conf = runApiaryT conf id

getRunner :: Monad m => ApiaryT c m (ActionT m a -> ActionT IO a)
getRunner = ApiaryT $ \run _ _ c -> c (hoistActionT run) mempty

getGuard :: ApiaryT c m (ActionT IO (SList c))
getGuard = ApiaryT $ \_ grd _ c -> c grd mempty

apiaryConfig :: ApiaryT c m ApiaryConfig
apiaryConfig = ApiaryT $ \_ _ c cont -> cont c mempty

addRoute :: ActionT IO () -> ApiaryT c m ()
addRoute r = ApiaryT $ \_ _ _ cont -> cont () r

focus :: Monad m => (SList c -> ActionT m (SList c')) -> ApiaryT c' m b -> ApiaryT c m b
focus g m = do
    tr <- getRunner
    ApiaryT $ \run grd cfg cont ->
        unApiaryT m run (grd >>= tr . g) cfg cont

action :: Monad m => Fn c (ActionT m ()) -> ApiaryT c m ()
action a = do
    tr  <- getRunner
    grd <- getGuard
    addRoute $ grd >>= tr . apply a

{-# DEPRECATED action_ "use action method." #-}
action_ :: Monad m => ActionT m () -> ApiaryT c m ()
action_ a = do
    tr <- getRunner
    grd <- getGuard
    addRoute $ grd >> tr a
