module Web.Apiary.Cookie 
    ( 
    -- * setter
      setCookie
    , deleteCookie
    -- * filter
    , cookie
    -- * Reexport
    -- | SetCookie(..)
    , module Web.Cookie
    ) where

import Web.Cookie (SetCookie(..))
import Web.Apiary.Cookie.Internal
