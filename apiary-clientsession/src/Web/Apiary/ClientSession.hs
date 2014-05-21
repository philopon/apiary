module Web.Apiary.ClientSession
    ( HasSession
    , SessionConfig(..)
    , withSession
    -- * setter
    , setSession
    , setSessionWith
    , setRawSession
    -- * filter
    , session
    -- * Reexport
    -- | def
    , module Data.Default.Class
    , module Web.Apiary.Cookie
    ) where

import Data.Default.Class
import Web.Apiary.ClientSession.Internal
import Web.Apiary.Cookie
