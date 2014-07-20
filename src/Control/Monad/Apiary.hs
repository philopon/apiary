module Control.Monad.Apiary
    ( ApiaryT, Apiary
    , runApiary
    , runApiaryT
    -- * getter
    , apiaryConfig
    -- * execute action
    , action, action'
    -- * API documentation
    , group
    , document
    , precondition
    , noDoc
    , rpHtml
    -- * deprecated
    , actionWithPreAction
    ) where

import Control.Monad.Apiary.Internal

import Data.Apiary.Document (rpHtml)
