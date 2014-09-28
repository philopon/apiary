{-# LANGUAGE CPP #-}

module Control.Monad.Apiary.Action 
    ( ActionT
    , ApiaryConfig(..)
    , defaultDocumentationAction
    , DefaultDocumentConfig(..)
    -- * actions
    , stop, stopWith

    -- ** getter
    , param
    , File(..)
    , getExt

    -- *** low level getter
    , getRequest
    , getHeaders
    , getParams
    , getQueryParams
    , getReqBodyParams
    , getReqBodyFiles

    -- ** setter
    , status
    -- *** response header
    , addHeader, setHeaders, modifyHeader
    , ContentType
    , contentType
    -- *** response body
    , reset
    , file
    , file'
    , builder
    , bytes, lazyBytes
    , text,  lazyText
    , showing
    , string, char
    , stream
    , rawResponse

    , StreamingBody

    -- ** monolithic action
    -- *** redirect
    , redirect, redirectPermanently, redirectTemporary
    , redirectWith
   
    -- * deprecated
    , getReqParams, getReqFiles
    , redirectFound, redirectSeeOther, source, lbs
    ) where

import Control.Monad.Apiary.Action.Internal

import Data.Apiary.Param
import Data.Apiary.Document.Html

#ifdef WAI3
import Network.Wai
#endif
