{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Web.Apiary.Session
    ( Session
    , getSession, setSession, deleteSession
    , session, session'
    , Proxy(Proxy)
    ) where

import Control.Monad(mzero)

import Control.Monad.Apiary.Action(ActionT)
import Control.Monad.Apiary.Filter(focus, Filter, Doc(DocPrecondition))

import Web.Apiary.Session.Internal
    (Session(Session), backendGet, backendSet, backendDelete)

import Data.Apiary.Extension(Has, getExt)
import Data.Apiary.Compat(Proxy(Proxy), KnownSymbol)
import qualified Data.Apiary.Dict as Dict
import qualified Data.Apiary.Router as R

-- | get session provided type.
getSession :: (Has (Session sess m) exts, Monad m) => proxy sess -> ActionT exts prms m (Maybe sess)
getSession _ = do
    Session b <- getExt Proxy
    backendGet b

-- | set session provided type.
setSession :: (Has (Session sess m) exts, Monad m) => proxy sess -> sess -> ActionT exts prms m ()
setSession _ v = do
    Session b <- getExt Proxy
    backendSet b v

-- | delete session provided type.
deleteSession :: forall proxy exts prms m sess. (Has (Session sess m) exts, Monad m)
              => proxy sess -> ActionT exts prms m ()
deleteSession _ = do
    Session b <- getExt (Proxy :: Proxy (Session sess m))
    backendDelete b

-- | filter by has session or not.
session' :: (Has (Session sess actM) exts, KnownSymbol key, Monad actM, key Dict.</ kvs)
         => kProxy key -> sProxy sess
         -> Filter exts actM m kvs (key Dict.:= sess ': kvs)
session' ky p = focus (DocPrecondition "session cookie required.") Nothing $ R.raw "session" $ \d t ->
    getSession p >>= \case
        Nothing -> mzero
        Just s  -> return (Dict.add ky s d, t)

-- | filter by has session or not. use \"session\" dict key.
--
-- @
-- session = session' (Proxy :: Proxy "session")
-- @
session :: (Has (Session sess actM) exts, Monad actM, "session" Dict.</ kvs)
        => proxy sess
        -> Filter exts actM m kvs ("session" Dict.:= sess ': kvs)
session = session' (Proxy :: Proxy "session")
