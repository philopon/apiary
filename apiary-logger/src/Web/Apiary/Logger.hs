module Web.Apiary.Logger (
    LogDest(..), LogConfig(..)
    , HasLogger, withLogger, logging
    , GivenLoggerT(..)
    ) where

import Web.Apiary.Logger.Internal

