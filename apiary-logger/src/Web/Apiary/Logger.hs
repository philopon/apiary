module Web.Apiary.Logger (
    -- * configuration
    LogDest(..), LogConfig(..), HasLogger
    -- * initialize
    , withLogger
    -- * action
    , logging
    -- * wrapper
    , GivenLoggerT(..)

    -- * reexports
    , module Data.Default.Class
    ) where

import Web.Apiary.Logger.Internal
import Data.Default.Class
