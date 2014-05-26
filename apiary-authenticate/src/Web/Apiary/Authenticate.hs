module Web.Apiary.Authenticate (
    AuthConfig(..), Provider(..)
    , OpenId_(..), OpenId
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

import Web.Apiary.Authenticate.Internal
import Data.Default.Class
