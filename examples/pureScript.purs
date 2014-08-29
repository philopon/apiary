module Main where

import Control.Monad.Eff
import Network.XHR
import Debug.Trace

main =
    post defaultAjaxOptions { onReadyStateChange = onSuccess $ \res -> do
        txt <- getResponseText res
        print txt
    } "/api/12" {} (urlEncoded {test: 24})
