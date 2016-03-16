{-# LANGUAGE ExplicitNamespaces #-}

module Web.Apiary
    ( module Control.Monad.Apiary
    , module Control.Monad.Apiary.Action
    , module Control.Monad.Apiary.Filter
    -- | File(..), Proxies
    , module Data.Apiary.Param

    -- | Method(..)
    , module Data.Apiary.Method
    -- | Has, MonadHas, Extensions, Initializer, Initializer', (+>), noExtension
    , module Data.Apiary.Extension
    -- | Member, Members, NotMember, Elem((:=))
    , module Network.Routing.Dict
    , key

    -- | hiding mkStatus
    , module Network.HTTP.Types.Status
    -- | def
    , module Data.Default.Class
    -- | MonadIO
    , module Control.Monad.IO.Class
    -- | MonadPlus(..), msum, mfilter, guard, (>=>)
    , module Control.Monad
    -- | FilePart(..), Application
    , module Network.Wai
    -- | Html
    , module Text.Blaze.Html
    ) where

import Control.Monad.Apiary
    ( ApiaryT
    , runApiaryTWith
    , runApiaryWith
    , runApiary
    , getApiaryTWith
    , getApiaryWith
    , getApiary
    , ApiaryConfig(..)
    , action
    , middleware
    , group
    , document
    , precondition
    , noDoc
    )

import Control.Monad.Apiary.Action
    ( ActionT
    , stop
    , param
    , params
    , status
    , addHeader, setHeaders, modifyHeader
    , contentType
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
    , file, file'
    , redirect, redirectPermanently, redirectTemporary
    , defaultDocumentationAction
    , DefaultDocumentConfig(..)
    , hoistActionT
    )

import Control.Monad.Apiary.Filter
    ( method
    , http09, http10, http11
    , root, capture
    , (??)
    , (=:), (=!:), (=?:), (=?!:), (=*:), (=+:)
    , switchQuery
    , eqHeader
    , header
    , accept
    , ssl
    )

import Data.Apiary.Param
    ( File(..)
    , pBool, pInt, pWord, pDouble
    , pText, pLazyText, pByteString, pLazyByteString, pString
    , pMaybe, pFile
    , pFirst, pOne, pMany, pSome, pOption, pOptional
    )

import Data.Apiary.Method(Method(..))
import Data.Apiary.Extension(Has, MonadExts(..), getExt, Extensions, Initializer, Initializer', (+>), noExtension)
import Network.Routing.Dict (Members, type (</), KV((:=)))

import Network.HTTP.Types.Status hiding (mkStatus)
import Data.Default.Class(def)
import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad (MonadPlus(..), msum, mfilter, guard, (>=>))
import Network.Wai(FilePart(..), Application)
import Text.Blaze.Html(Html)
import Web.Apiary.TH(key)
