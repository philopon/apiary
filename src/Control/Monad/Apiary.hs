module Control.Monad.Apiary
    ( ApiaryT, Apiary
    , runApiary
    , runApiaryT
    , runApiaryTWith
    , runApiaryWith
    -- * getter
    , apiaryConfig
    , apiaryExt
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

import Data.Apiary.Document.Html (rpHtml)
