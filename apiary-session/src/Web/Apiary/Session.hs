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

import Control.Monad.Apiary(ApiaryT)
import Control.Monad.Apiary.Action(ActionT, getParams)
import Control.Monad.Apiary.Filter(focus, Doc(DocPrecondition))

import Web.Apiary.Session.Internal
    (Session(Session), backendGet, backendSet, backendDelete)

import Data.Apiary.Extension(Has, getExt)
import Data.Apiary.Compat(Proxy(Proxy), KnownSymbol)
import qualified Data.Apiary.Dict as Dict

getSession :: (Has (Session sess m) exts, Monad m) => proxy sess -> ActionT exts prms m (Maybe sess)
getSession _ = do
    Session b <- getExt Proxy
    backendGet b

setSession :: (Has (Session sess m) exts, Monad m) => proxy sess -> sess -> ActionT exts prms m ()
setSession _ v = do
    Session b <- getExt Proxy
    backendSet b v

deleteSession :: forall proxy exts prms m sess. (Has (Session sess m) exts, Monad m)
              => proxy sess -> ActionT exts prms m ()
deleteSession _ = do
    Session b <- getExt (Proxy :: Proxy (Session sess m))
    backendDelete b

session' :: (Has (Session sess actM) exts, KnownSymbol key, Monad actM, Dict.NotMember key kvs)
         => kProxy key -> sProxy sess
         -> ApiaryT exts (key Dict.:= sess ': kvs) actM m ()
         -> ApiaryT exts kvs actM m ()
session' ky p = focus (DocPrecondition "session cookie required.") $ do
    dict <- getParams
    getSession p >>= \case
        Nothing -> mzero
        Just s  -> return $ Dict.insert ky s dict

session :: (Has (Session sess actM) exts, Monad actM, Dict.NotMember "session" kvs)
        => proxy sess
        -> ApiaryT exts ("session" Dict.:= sess ': kvs) actM m ()
        -> ApiaryT exts kvs actM m ()
session = session' (Proxy :: Proxy "session")
