module Control.Monad.Apiary
    ( ApiaryT, Apiary
    , runApiary
    , runApiaryT
    , runApiaryT'
    -- * getter
    , apiaryConfig
    -- * execute action
    , action, action'
    -- * API documentation
    , group
    , document
    , precondition
    , cap
    -- * deprecated
    , actionWithPreAction
    ) where

import Control.Monad.Apiary.Internal
import Data.Apiary.Document (cap)
