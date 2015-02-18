{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}

module Web.Apiary.Authenticate
    ( I.Auth
    , I.AuthConfig(..), I.Provider(..)
    , I.OpenId_(..), I.OpenId, pOpenId
    -- * initializer
    , initAuth, initAuthWith, initAuthWithManager
    -- * handler
    , authHandler
    -- * filter
    , authorized, authorized'
    -- * action
    , I.authLogout
    -- ** getter
    , authConfig, authProviders, authRoutes
    ) where

import Web.Apiary
import qualified Web.Apiary.Authenticate.Internal as I
import qualified Network.Routing.Dict as Dict
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Web.Apiary.Session
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Apiary.Filter(Filter)

import qualified Data.Text as T
import qualified Data.ByteString as S

import Data.Apiary.Compat
import Data.Apiary.Extension

pOpenId :: Proxy I.OpenId
pOpenId = Proxy

initAuthWithManager :: (Has (Session I.OpenId m) exts, MonadBaseControl IO m)
                    => Client.Manager -> I.AuthConfig
                    -> Initializer m exts (I.Auth ': exts)
initAuthWithManager mgr conf = initializer' $ return (I.Auth mgr conf)

initAuthWith :: (Has (Session I.OpenId m) exts, MonadBaseControl IO m)
             => Client.ManagerSettings -> I.AuthConfig
             -> Initializer m exts (I.Auth ': exts)
initAuthWith ms conf = initializerBracket' $ \m ->
    control $ \run -> Client.withManager ms (\mgr -> run . m $ I.Auth mgr conf)

initAuth :: (Has (Session I.OpenId m) exts, MonadBaseControl IO m)
         => I.AuthConfig -> Initializer m exts (I.Auth ': exts)
initAuth = initAuthWith tlsManagerSettings

-- | default auth handlers. since 0.8.0.0.
authHandler :: (Monad m, MonadIO actM, Has I.Auth exts, Has (Session I.OpenId actM) exts)
            => ApiaryT exts prms actM m ()
authHandler = getExt (Proxy :: Proxy I.Auth) >>= I.authHandler 

authorized' :: (Has (Session I.OpenId actM) exts, KnownSymbol key, Monad actM, key Dict.</ kvs)
            => proxy key
            -> Filter exts actM m kvs (key := I.OpenId ': kvs)
authorized' ky = session' ky (Proxy :: Proxy I.OpenId)

-- | filter which check whether logged in or not, and get id. since 0.7.0.0.
authorized :: (Has (Session I.OpenId actM) exts, Monad actM, "auth" Dict.</ kvs)
           => Filter exts actM m kvs ("auth" := I.OpenId ': kvs)
authorized = authorized' (Proxy :: Proxy "auth")

authConfig :: (Has I.Auth exts, Monad m) => ActionT exts prms m I.AuthConfig
authConfig = I.config `liftM` getExt (Proxy :: Proxy I.Auth)

authProviders :: (Has I.Auth exts, Monad m) => ActionT exts prms m [(T.Text, I.Provider)]
authProviders = I.authProviders `liftM` getExt (Proxy :: Proxy I.Auth)

-- | get authenticate routes: (title, route). since 0.7.0.0.
authRoutes :: (Has I.Auth exts, Monad m) => ActionT exts prms m [(T.Text, S.ByteString)]
authRoutes = I.authRoutes `liftM` getExt (Proxy :: Proxy I.Auth)
