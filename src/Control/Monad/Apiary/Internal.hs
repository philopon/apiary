{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Apiary.Internal where

import Network.Wai
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader

import Control.Monad.Apiary.Action.Internal

newtype ApiaryT c m a = ApiaryT { unApiaryT ::
         ReaderT (forall b. m b -> IO b) 
        (ReaderT (ActionT IO c) 
        (ReaderT ApiaryConfig
        (Writer  (ActionT IO ())))) a 
    } deriving (Functor, Applicative, Monad)

type Apiary c = ApiaryT c IO

-- TODO: error when add signature
runApiaryT config run (ApiaryT m) =
    execActionT config . execWriter . flip runReaderT config $ runReaderT (runReaderT m run) (return ())

runApiary :: ApiaryConfig -> Apiary () a -> Application
runApiary config = runApiaryT config id

focus :: Monad m => (c -> ActionT m c') -> ApiaryT c' m b -> ApiaryT c m b
focus f (ApiaryT m) = do
    tr <- hoistActionT `fmap` ApiaryT ask
    ApiaryT . ReaderT $ \r -> ReaderT $ \c -> runReaderT (runReaderT m r) (c >>= \a -> tr (f a))

action_ :: Monad m => ActionT m () -> ApiaryT c m ()
action_ = action . const

action :: Monad m => (c -> ActionT m ()) -> ApiaryT c m ()
action a = do
    tr   <- hoistActionT `fmap` ApiaryT ask
    ApiaryT $ lift ask >>= \g -> (lift . lift . lift) (tell $ g >>= \c -> tr (a c))

apiaryConfig :: ApiaryT c m ApiaryConfig
apiaryConfig = ApiaryT . lift $ lift ask
