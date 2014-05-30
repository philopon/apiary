{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Web.Apiary.Authenticate (
    E.AuthConfig(..), E.Provider(..)
    , E.OpenId_(..), E.OpenId
    , HasAuth
    , withAuth, withAuthWith
    , authHandler
    -- * filter
    , authorized
    -- * action
    , authLogout
    -- ** getter
    , authConfig, authProviders, authRoutes
    -- * reexport
    , module Data.Default.Class
    ) where

import Web.Apiary
import qualified Web.Apiary.Authenticate.Explicit as E
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Web.Apiary.ClientSession

import Data.Reflection
import Data.Default.Class
import Data.Apiary.SList
import qualified Data.Text as T
import qualified Data.ByteString as S

type HasAuth = Given E.Auth

withAuth :: HasSession => E.AuthConfig -> (HasAuth => IO a) -> IO a
withAuth = withAuthWith tlsManagerSettings

withAuthWith :: HasSession => Client.ManagerSettings
             -> E.AuthConfig -> (HasAuth => IO a) -> IO a
withAuthWith ms ac m = E.withAuthWith given ms ac (\a -> give a m)

authHandler :: (Functor n, MonadIO n, HasAuth) => ApiaryT c n m ()
authHandler = E.authHandler given

authorized :: HasAuth => Apiary (Snoc as E.OpenId) a -> Apiary as a
authorized = E.authorized given

authLogout :: (Monad m, HasAuth) => ActionT m ()
authLogout = E.authLogout given

authConfig :: HasAuth => E.AuthConfig
authConfig = E.authConfig given

authProviders :: HasAuth => [(T.Text, E.Provider)]
authProviders = E.authProviders given

authRoutes :: HasAuth => [(T.Text, S.ByteString)]
authRoutes = E.authRoutes given
