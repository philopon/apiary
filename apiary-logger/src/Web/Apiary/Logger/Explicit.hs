module Web.Apiary.Logger.Explicit (
    -- * configuration
    LogDest(..), LogConfig(..), Logger
    -- * initialize
    , withLogger
    -- * action
    , logging
    -- * reexports
    , module Data.Default.Class
    ) where

import Web.Apiary.Logger.Internal
import Data.Default.Class
