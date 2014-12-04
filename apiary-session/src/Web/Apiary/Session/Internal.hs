{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}

module Web.Apiary.Session.Internal
    ( Session(..)
    , SessionBackend(..)
    ) where

import Control.Monad.Apiary.Action(ActionT)
import qualified Network.Wai as Wai
import Data.Apiary.Extension
    (Extension(extMiddleware, extMiddleware'), Middleware')

data Session sess m = forall backend. SessionBackend backend sess m => Session
    { sessionBackend :: backend }

instance Extension (Session sess m) where
    extMiddleware  (Session back) = backendMiddleware  back
    extMiddleware' (Session back) = backendMiddleware' back

class Monad m => SessionBackend backend sess m | backend -> sess, backend -> m where
    backendMiddleware :: backend -> Wai.Middleware
    backendMiddleware _ = id
    {-# INLINE backendMiddleware #-}

    backendMiddleware' :: backend -> Middleware'
    backendMiddleware' _ = id
    {-# INLINE backendMiddleware' #-}

    genBackendModify  :: backend
                      -> (Maybe sess -> ActionT exts prms m (Maybe sess, a))
                      -> ActionT exts prms m a

    backendGet :: backend -> ActionT exts prms m (Maybe sess)
    backendGet b = genBackendModify b (\s -> return (s, s))

    backendSet :: backend -> sess -> ActionT exts prms m ()
    backendSet b s = genBackendModify b (\_ -> return (Just s, ()))

    backendDelete :: backend -> ActionT exts prms m ()
    backendDelete b = genBackendModify b (\_ -> return (Nothing, ()))
