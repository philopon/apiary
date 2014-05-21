module Web.Apiary.Authenticate (
    AuthConfig(..), Provider(..)
    , HasAuth
    , withAuth, withAuthWith
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
