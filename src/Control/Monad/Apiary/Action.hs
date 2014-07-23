{-# LANGUAGE CPP #-}

module Control.Monad.Apiary.Action 
    ( ActionT
    , ApiaryConfig(..)
    , defaultDocumentationAction
    , DefaultDocumentConfig(..)
    -- * actions
    , stop, stopWith

    -- ** getter
    , getRequest
    , getHeaders
    , getReqParams
    , File(..)
    , getReqFiles

    -- ** setter
    , status
    -- *** response header
    , addHeader, setHeaders, modifyHeader
    , ContentType
    , contentType
    -- *** response body
    , file
    , file'
    , builder
    , lbs
    , stream
    , rawResponse

    , StreamingBody

    -- ** monolithic action
    -- *** redirect
    , redirect, redirectPermanently, redirectTemporary
    , redirectWith
   
    -- * deprecated
    , redirectFound, redirectSeeOther, source
    ) where

import Control.Monad.Apiary.Action.Internal

import Data.Apiary.Param
import Data.Apiary.Document

#ifdef WAI3
import Network.Wai
#endif
