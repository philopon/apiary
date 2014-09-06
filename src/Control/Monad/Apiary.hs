module Control.Monad.Apiary
    ( ApiaryT
    , runApiaryTWith
    , runApiaryWith
    , runApiary
    -- * getter
    , apiaryConfig
    , apiaryExt
    -- * execute action
    , action, action'
    -- * middleware
    , middleware
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

import Data.Apiary.Document.Html (rpHtml)
