module Web.Apiary.ClientSession
    ( HasSession
    , SessionConfig(..)
    , withSession
    -- * setter
    , setSession
    -- * filter
    , session
    -- * Reexport
    -- | def
    , module Data.Default.Class
    ) where

import Data.Default.Class
import Web.Apiary.ClientSession.Internal
