module Web.Apiary.WebSockets (
      actionWithWebSockets 
    , actionWithWebSockets'
    -- * Reexport
    -- | pandingRequest, acceptRequest, rejectrequest
    --
    -- receiveData
    --
    -- sendTextData, sendBinaryData, sendClose, sendPing
    , module Network.WebSockets
    ) where

import Web.Apiary
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS

import Network.WebSockets
    ( pendingRequest, acceptRequest, rejectRequest
    , receiveData
    , sendTextData, sendBinaryData, sendClose, sendPing
    )

actionWithWebSockets' :: Monad m 
                      => WS.ConnectionOptions
                      -> WS.ServerApp
                      -> Fn c (ActionT m ())
                      -> ApiaryT c m ()
actionWithWebSockets' conn srv m = do
    actionWithPreAction pa m
  where
    pa = do
        req <- getRequest
        case WS.websocketsApp conn srv req of
            Nothing   -> return ()
            Just resp -> stopWith resp

actionWithWebSockets :: Monad m 
                     => WS.ServerApp
                     -> Fn c (ActionT m ())
                     -> ApiaryT c m ()
actionWithWebSockets = actionWithWebSockets' WS.defaultConnectionOptions
