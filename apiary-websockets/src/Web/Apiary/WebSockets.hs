{-# LANGUAGE NoMonomorphismRestriction #-}
module Web.Apiary.WebSockets (
      actionWithWebSockets 
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

actionWithWebSockets' :: Monad m => WS.ConnectionOptions 
                      -> Fn xs WS.ServerApp
                      -> Fn xs (ActionT m ())
                      -> ApiaryT xs m ()
actionWithWebSockets' conn srv m = do
    actionWithPreAction pa m
  where
    pa args = do
        req <- getRequest
        case WS.websocketsApp conn (apply srv args) req of
            Nothing   -> return ()
            Just resp -> stopWith resp

actionWithWebSockets :: Monad m 
                     => Fn c WS.ServerApp
                     -> Fn c (ActionT m ())
                     -> ApiaryT c m ()
actionWithWebSockets = actionWithWebSockets' WS.defaultConnectionOptions
