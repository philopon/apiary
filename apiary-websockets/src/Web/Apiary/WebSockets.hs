module Web.Apiary.WebSockets (
    webSockets, webSockets'
    , actionWithWebSockets 
    , actionWithWebSockets'
    , websocketsToAction
    -- * Reexport
    -- | PendingConnection,
    -- pandingRequest, acceptRequest, rejectrequest
    --
    -- receiveData
    --
    -- sendTextData, sendBinaryData, sendClose, sendPing
    , module Network.WebSockets
    ) where

import Control.Monad(mzero, mplus)
import Control.Monad.Apiary(ApiaryT, action)
import Control.Monad.Apiary.Action(ActionT, getRequest, getParams, stopWith)

import qualified Network.Routing.Dict as Dict
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS

import Network.WebSockets
    ( PendingConnection
    , pendingRequest, acceptRequest, rejectRequest
    , receiveData
    , sendTextData, sendBinaryData, sendClose, sendPing
    )

websocketsToAction :: Monad m => WS.ConnectionOptions 
                   -> (Dict.Dict prms -> WS.ServerApp) -> ActionT exts prms m ()
websocketsToAction conn srv = do
    req <- getRequest
    d   <- getParams
    case WS.websocketsApp conn (srv d) req of
        Nothing -> mzero
        Just r  -> stopWith r

-- | websocket only action. since 0.8.0.0.
webSockets' :: (Monad m, Monad actM) => WS.ConnectionOptions
            -> (Dict.Dict prms -> WS.ServerApp) -> ApiaryT exts prms actM m ()
webSockets' conn srv = action $ websocketsToAction conn srv

-- | websocket only action. since 0.8.0.0.
webSockets :: (Monad m, Monad n)
           => (Dict.Dict prms -> WS.ServerApp) -> ApiaryT exts prms n m ()
webSockets = webSockets' WS.defaultConnectionOptions

actionWithWebSockets' :: (Monad m, Monad actM)
                      => WS.ConnectionOptions 
                      -> (Dict.Dict prms -> WS.ServerApp)
                      -> ActionT exts prms actM ()
                      -> ApiaryT exts prms actM m ()
actionWithWebSockets' conn srv m =
    action $ websocketsToAction conn srv `mplus` m

actionWithWebSockets :: (Monad m, Monad actM)
                     => (Dict.Dict prms -> WS.ServerApp)
                     -> ActionT exts prms actM ()
                     -> ApiaryT exts prms actM m ()
actionWithWebSockets = actionWithWebSockets' WS.defaultConnectionOptions
