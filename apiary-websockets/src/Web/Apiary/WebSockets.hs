{-# LANGUAGE NoMonomorphismRestriction #-}
module Web.Apiary.WebSockets (
    webSockets, webSockets'
    , actionWithWebSockets 
    , actionWithWebSockets'
    -- * Reexport
    -- | PendingConnection,
    -- pandingRequest, acceptRequest, rejectrequest
    --
    -- receiveData
    --
    -- sendTextData, sendBinaryData, sendClose, sendPing
    , module Network.WebSockets
    ) where

import Web.Apiary
import Data.Apiary.SList
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS

import Network.WebSockets
    ( PendingConnection
    , pendingRequest, acceptRequest, rejectRequest
    , receiveData
    , sendTextData, sendBinaryData, sendClose, sendPing
    )

wsToApp :: Monad m => WS.ConnectionOptions 
        -> Fn xs WS.ServerApp -> SList xs -> ActionT exts m ()
wsToApp conn srv args = getRequest >>= \req ->
    case WS.websocketsApp conn (apply srv args) req of
        Nothing -> return ()
        Just r  -> stopWith r

-- | websocket only action. since 0.8.0.0.
webSockets' :: (Functor m, Monad m, Functor actM, Monad actM) => WS.ConnectionOptions
            -> Fn prms WS.ServerApp -> ApiaryT exts prms actM m ()
webSockets' conn srv = action' $ wsToApp conn srv

-- | websocket only action. since 0.8.0.0.
webSockets :: (Functor m, Monad m, Functor n, Monad n)
           => Fn xs WS.ServerApp -> ApiaryT exts xs n m ()
webSockets = webSockets' WS.defaultConnectionOptions

actionWithWebSockets' :: (Functor m, Monad m, Functor actM, Monad actM)
                      => WS.ConnectionOptions 
                      -> Fn prms WS.ServerApp
                      -> Fn prms (ActionT exts actM ())
                      -> ApiaryT exts prms actM m ()
actionWithWebSockets' conn srv m =
    action' $ \a -> wsToApp conn srv a >> apply m a

actionWithWebSockets :: (Functor m, Monad m, Functor n, Monad n)
                     => Fn c WS.ServerApp
                     -> Fn c (ActionT exts n ())
                     -> ApiaryT exts c n m ()
actionWithWebSockets = actionWithWebSockets' WS.defaultConnectionOptions
