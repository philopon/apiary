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
    , file

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
    , getExt
    , getQueryParams
    , getReqBodyParams
    , getReqBodyFiles
    -- ** setter
    , file'
    , stream
    , rawResponse
    , StreamingBody
    -- ** redirect
    , redirectWith
    ) where

import Control.Monad.Apiary.Action.Internal

import Data.Apiary.Document.Html

#ifdef WAI3
import Network.Wai
#endif
