{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Apiary.Document
import Data.Monoid
import qualified Data.Text as T

import Control.Monad.Apiary.Action.Internal

data ApiaryReader n c = ApiaryReader
    { readerFilter :: ActionT n (SList c)
    , readerConfig :: ApiaryConfig
    , readerDoc    :: Doc -> Doc
    }

data ApiaryWriter n = ApiaryWriter
    { writerHandler :: ActionT n ()
    , writerDoc     :: [Doc]
    }

instance Monad n => Monoid (ApiaryWriter n) where
    mempty = ApiaryWriter mzero []
    ApiaryWriter ah ad `mappend` ApiaryWriter bh bd =
        ApiaryWriter (mplus ah bh) (ad <> bd)

initialReader :: Monad n => ApiaryConfig -> ApiaryReader n '[]
initialReader conf = ApiaryReader (return SNil) conf id

-- | most generic Apiary monad. since 0.8.0.0.
newtype ApiaryT c n m a = ApiaryT { unApiaryT :: forall b.
    ApiaryReader n c
    -> (a -> ApiaryWriter n -> m b)
    -> m b 
    }

-- | no transformer. (ActionT IO, ApiaryT Identity)
type Apiary c = ApiaryT c IO Identity

instance Functor (ApiaryT c n m) where
    fmap f m = ApiaryT $ \rdr cont ->
        unApiaryT m rdr $ \a hdr -> hdr `seq` cont (f a) hdr

instance Monad n => Applicative (ApiaryT c n m) where
    pure x = ApiaryT $ \_ cont -> cont x mempty
    mf <*> ma = ApiaryT $ \rdr cont ->
        unApiaryT mf rdr $ \f hdr  ->
        unApiaryT ma rdr $ \a hdr' ->
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont (f a) hdr''

instance Monad n => Monad (ApiaryT c n m) where
    return x = ApiaryT $ \_ cont -> cont x mempty
    m >>= k = ApiaryT $ \rdr cont ->
        unApiaryT    m  rdr $ \a hdr  ->
        unApiaryT (k a) rdr $ \b hdr' -> 
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont b hdr''

instance Monad n => MonadTrans (ApiaryT c n) where
    lift m = ApiaryT $ \_ c -> m >>= \a -> c a mempty

instance (Monad n, MonadIO m) => MonadIO (ApiaryT c n m) where
    liftIO m = ApiaryT $ \_ c -> liftIO m >>= \a -> c a mempty

instance (Monad n, MonadBase b m) => MonadBase b (ApiaryT c n m) where
    liftBase m = ApiaryT $ \_ c -> liftBase m >>= \a -> c a mempty

apiaryT :: Monad m
        => (ApiaryReader n c -> m (a, ApiaryWriter n))
        -> ApiaryT c n m a
apiaryT f = ApiaryT $ \rdr cont -> f rdr >>= \(a,w) -> cont a w

instance Monad n => MonadTransControl (ApiaryT c n) where
    newtype StT (ApiaryT c n) a = StTApiary' { unStTApiary' :: (a, ApiaryWriter n) }
    liftWith f = apiaryT $ \rdr ->
        liftM (\a -> (a, mempty)) 
        (f $ \t -> liftM StTApiary' $ unApiaryT t rdr (\a w -> return (a,w)))
    restoreT m = apiaryT $ \_ -> liftM unStTApiary' m

instance (Monad n, MonadBaseControl b m) => MonadBaseControl b (ApiaryT c n m) where
    newtype StM (ApiaryT c n m) a = StMApiary' { unStMApiary' :: ComposeSt (ApiaryT c n) m a }
    liftBaseWith = defaultLiftBaseWith StMApiary'
    restoreM     = defaultRestoreM   unStMApiary'

runApiaryT' :: (Monad n, Monad m) => (forall b. n b -> IO b) -> ApiaryConfig
            -> ApiaryT '[] n m a -> m (Application, Documents)
runApiaryT' run conf m = unApiaryT m (initialReader conf) (\_ w -> return w) >>= \wtr -> do
    let doc = docsToDocuments $ writerDoc wtr
        da  = maybe mzero (\f -> f doc) $ documentationAction conf
        app = execActionT conf $ hoistActionT run (writerHandler wtr) `mplus` da
    return (app, doc)

runApiaryT :: (Monad n, Monad m) => (forall b. n b -> IO b) -> ApiaryConfig
           -> ApiaryT '[] n m a -> m Application
runApiaryT run conf m = fst `liftM` runApiaryT' run conf m

runApiary :: ApiaryConfig -> Apiary '[] a -> Application
runApiary conf m = runIdentity $ runApiaryT id conf m

apiaryConfig :: Monad n => ApiaryT c n m ApiaryConfig
apiaryConfig = ApiaryT $ \r cont -> cont (readerConfig r) mempty

addRoute :: Monad n => ApiaryWriter n -> ApiaryT c n m ()
addRoute r = ApiaryT $ \_ cont -> cont () r

-- | filter by action. since 0.6.1.0.
focus :: Monad n => (Doc -> Doc) -> (SList c -> ActionT n (SList c'))
      -> ApiaryT c' n m a -> ApiaryT c n m a
focus d g m = ApiaryT $ \rdr cont -> unApiaryT m rdr 
    { readerFilter = readerFilter rdr >>= g 
    , readerDoc    = readerDoc rdr . d
    } cont

-- | splice ActionT ApiaryT.
action :: Monad n => Fn c (ActionT n ()) -> ApiaryT c n m ()
action = action' . apply

-- | API document group. since 0.12.0.0.
--
-- only top level group recognized.
group :: T.Text -> ApiaryT c n m a -> ApiaryT c n m a
group d m = ApiaryT $ \rdr cont -> unApiaryT m rdr
    { readerDoc = readerDoc rdr . DocGroup d } cont

-- | add API document. since 0.12.0.0.
--
-- It use only filters prior document,
-- so you should be placed document directly in front of action.
document :: T.Text -> ApiaryT c n m a -> ApiaryT c n m a
document d m = ApiaryT $ \rdr cont -> unApiaryT m rdr
    { readerDoc = \_ -> readerDoc rdr (Document $ Just d) } cont

{-# DEPRECATED actionWithPreAction "use action'" #-}
-- | execute action before main action. since 0.4.2.0
actionWithPreAction :: Monad n => (SList xs -> ActionT n a)
                    -> Fn xs (ActionT n ()) -> ApiaryT xs n m ()
actionWithPreAction pa a = do
    action' $ \c -> pa c >> apply a c

getReader :: Monad n => ApiaryT c n m (ApiaryReader n c)
getReader = ApiaryT $ \rdr cont -> cont rdr mempty

-- | like action. but not apply arguments. since 0.8.0.0.
action' :: Monad n => (SList c -> ActionT n ()) -> ApiaryT c n m ()
action' a = do
    rdr <- getReader
    addRoute $ ApiaryWriter (readerFilter rdr >>= \c -> a c) 
        [readerDoc rdr $ Document Nothing]
