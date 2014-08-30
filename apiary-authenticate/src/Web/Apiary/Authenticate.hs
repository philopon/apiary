{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web.Apiary.Authenticate
    ( I.Auth
    , I.AuthConfig(..), I.Provider(..)
    , I.OpenId_(..), I.OpenId
    -- * initializer
    , initAuth, initAuthWith, initAuthWithManager
    -- * handler
    , authHandler
    -- * filter
    , authorized
    -- * action
    , authLogout
    -- ** getter
    , authConfig, authProviders, authRoutes
    ) where

import Web.Apiary
import qualified Web.Apiary.Authenticate.Internal as I
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Web.Apiary.ClientSession
import Control.Monad
import Control.Monad.Trans.Control

import qualified Data.Text as T
import qualified Data.ByteString as S

import Data.Apiary.Proxy
import Data.Apiary.Extension
import Data.Apiary.Extension.Internal

initAuthWithManager :: (Has Session exts, MonadBaseControl IO m)
                    => Client.Manager -> I.AuthConfig
                    -> Initializer m exts (I.Auth ': exts)
initAuthWithManager mgr conf = Initializer $ \e -> do
    I.authWith mgr conf $ \a -> return $ addExtension a e

initAuthWith :: (Has Session exts, MonadBaseControl IO m)
             => Client.ManagerSettings -> I.AuthConfig
             -> Initializer m exts (I.Auth ': exts)
initAuthWith ms conf = Initializer $ \e -> do
    control $ \run -> Client.withManager ms $ \mgr ->
        I.authWith mgr conf $ \a -> run . return $ addExtension a e

initAuth :: (Has Session exts, MonadBaseControl IO m)
         => I.AuthConfig -> Initializer m exts (I.Auth ': exts)
initAuth = initAuthWith tlsManagerSettings

-- | default auth handlers. since 0.8.0.0.
authHandler :: (Monad m, MonadIO actM, Has I.Auth exts, Has Session exts)
            => ApiaryT exts prms actM m ()
authHandler = apiaryExt (Proxy :: Proxy I.Auth) >>= I.authHandler 

-- | filter which check whether logged in or not, and get id. since 0.7.0.0.
authorized :: (Has I.Auth exts, MonadIO actM, Has Session exts)
           => ApiaryT exts (I.OpenId ': prms) actM m () -> ApiaryT exts prms actM m ()
authorized m = do
    a <- apiaryExt (Proxy :: Proxy I.Auth)
    I.authorized a m

-- | delete session. since 0.7.0.0.
authLogout :: (Monad m, Has I.Auth exts) => ActionT exts m ()
authLogout = getExt (Proxy :: Proxy I.Auth) >>= I.authLogout

authConfig :: (Has I.Auth exts, Monad m) => ActionT exts m I.AuthConfig
authConfig = I.config `liftM` getExt (Proxy :: Proxy I.Auth)

authProviders :: (Has I.Auth exts, Monad m) => ActionT exts m [(T.Text, I.Provider)]
authProviders = I.authProviders `liftM` getExt (Proxy :: Proxy I.Auth)

-- | get authenticate routes: (title, route). since 0.7.0.0.
authRoutes :: (Has I.Auth exts, Monad m) => ActionT exts m [(T.Text, S.ByteString)]
authRoutes = I.authRoutes `liftM` getExt (Proxy :: Proxy I.Auth)
