{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Apiary.Documentation
    ( defaultDocumentationAction, DefaultDocumentConfig(..)
    , defaultDocumentation
    ) where

import Control.Monad.Apiary.Internal(ApiaryT, action)
import Control.Monad.Apiary.Action.Internal(ActionT, getDocuments, contentType, builder, lazyText)
import Control.Monad.Apiary.Filter(method, accept)

import Text.Blaze.Html.Renderer.Utf8(renderHtmlBuilder)

import Data.Apiary.Method(Method(..))
import Data.Apiary.Document.Html(DefaultDocumentConfig(..), defaultDocumentToHtml)
import Data.Apiary.Document.JSON(documentsToJSON)

-- | auto generated document.
defaultDocumentationAction :: Monad m => DefaultDocumentConfig -> ActionT exts prms m ()
defaultDocumentationAction conf = do
    d <- getDocuments
    contentType "text/html"
    builder . renderHtmlBuilder $ defaultDocumentToHtml conf d

defaultDocumentation :: Monad actM => DefaultDocumentConfig -> ApiaryT exts prms actM m ()
defaultDocumentation conf = method GET $ do
    accept "text/html" . action $
        getDocuments >>= builder . renderHtmlBuilder . defaultDocumentToHtml conf

    accept "application/json" . action $ do
        getDocuments >>= lazyText . documentsToJSON
