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
        -> Fn xs WS.ServerApp -> SList xs -> ActionT m ()
wsToApp conn srv args = getRequest >>= \req ->
    case WS.websocketsApp conn (apply srv args) req of
        Nothing -> return ()
        Just r  -> stopWith r

-- | websocket only action. since 0.8.0.0.
webSockets' :: (Monad n, Functor n) => WS.ConnectionOptions
            -> Fn xs WS.ServerApp -> ApiaryT xs n m ()
webSockets' conn srv = action' $ wsToApp conn srv

-- | websocket only action. since 0.8.0.0.
webSockets :: (Monad n, Functor n)
           => Fn xs WS.ServerApp -> ApiaryT xs n m ()
webSockets = webSockets' WS.defaultConnectionOptions

actionWithWebSockets' :: (Monad n, Functor n)
                      => WS.ConnectionOptions 
                      -> Fn xs WS.ServerApp
                      -> Fn xs (ActionT n ())
                      -> ApiaryT xs n m ()
actionWithWebSockets' conn srv m =
    action' $ \a -> wsToApp conn srv a >> apply m a

actionWithWebSockets :: (Functor n, Monad n)
                     => Fn c WS.ServerApp
                     -> Fn c (ActionT n ())
                     -> ApiaryT c n m ()
actionWithWebSockets = actionWithWebSockets' WS.defaultConnectionOptions
