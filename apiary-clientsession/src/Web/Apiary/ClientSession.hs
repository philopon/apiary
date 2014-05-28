{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.ClientSession
    ( HasSession
    , I.SessionConfig(..)
    , withSession
    -- * setter
    , setSession
    , csrfToken
    -- * filter
    , session
    , checkToken
    -- * Reexport
    , module Data.Default.Class
    -- | deleteCookie
    , module Web.Apiary.Cookie
    ) where

import Network.Wai
import Data.Default.Class
import qualified Web.Apiary.ClientSession.Internal as I
import Web.Apiary.Cookie (deleteCookie)
import Data.Reflection
import Control.Monad.Trans
import qualified Data.ByteString as S
import Web.Apiary
import Control.Monad.Apiary.Filter.Internal.Strategy
import Data.Proxy

type HasSession = Given I.Session

withSession :: MonadIO m => I.SessionConfig -> (HasSession => m b) -> m b
withSession c m = I.withSession c (\s -> give s m)

setSession :: (MonadIO m, HasSession)
           => S.ByteString -> S.ByteString -> ActionT m ()
setSession = I.setSession given

csrfToken :: (MonadIO m, HasSession) => ActionT m S.ByteString
csrfToken = I.csrfToken given

session :: (Functor n, MonadIO n, Strategy w, Query a, HasSession)
        => S.ByteString -> Proxy (w a)
        -> ApiaryT (SNext w as a) n m b -> ApiaryT as n m b
session = I.session given

checkToken :: (Functor n, MonadIO n, HasSession)
           => (Request -> Maybe S.ByteString) -- ^ token accessor
           -> ApiaryT c n m a -> ApiaryT c n m a
checkToken = I.checkToken given
