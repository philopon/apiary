{-# LANGUAGE CPP #-}

module Control.Monad.Apiary.Action 
    ( ActionT
    -- * stop action
    , stop

    -- * getter
    , param
    , params

    -- * setter
    , status
    -- ** response header
    , addHeader, setHeaders, modifyHeader
    , contentType
    -- ** response body
    , reset
    , builder
    , bytes, lazyBytes
    , text,  lazyText
    , showing
    , string, char
    , appendBuilder
    , appendBytes, appendLazyBytes
    , appendText, appendLazyText
    , appendShowing
    , appendString, appendChar
    , file
    , file'

    -- ** monolithic action
    -- *** redirect
    , redirect, redirectPermanently, redirectTemporary

    -- *** documentation
    , defaultDocumentationAction
    , DefaultDocumentConfig(..)

    -- * not export from Web.Apiary
    , ContentType
    , stopWith
    -- ** getter
    , getRequest
    , getHeaders
    , getParams
    , getQueryParams
    , getReqBodyParams
    , getReqBodyFiles
    -- ** setter
    , devFile
    , devFile'
    , stream
    , rawResponse
    , StreamingBody
    -- ** vault
    , lookupVault
    , modifyVault
    , insertVault
    , adjustVault
    , deleteVault
    -- ** redirect
    , redirectWith
    ) where

import Control.Monad.Apiary.Action.Internal

import Data.Apiary.Document.Html

#ifdef WAI3
import Network.Wai
#endif
