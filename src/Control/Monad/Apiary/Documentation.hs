{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Apiary.Documentation {-# WARNING TODO: implement #-}
    ( defaultDocumentationAction, DefaultDocumentConfig(..)
    , defaultDocumentation
    ) where

import Control.Monad.Apiary.Internal(ApiaryT, action)
import Control.Monad.Apiary.Action.Internal(ActionT, getDocuments, contentType, builder, lazyText)
import Control.Monad.Apiary.Filter(method, accept)

import Data.Apiary.Method(Method(..))
import Data.Apiary.Document.JSON(documentsToJSON)

type DefaultDocumentConfig = ()
defaultDocumentation = undefined
defaultDocumentationAction = undefined
