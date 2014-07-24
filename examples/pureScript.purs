module Main where

import Control.Monad.Eff
import Network.XHR
import Debug.Trace

main :: Eff (trace :: Trace, xhr :: XHR) Unit
main = do
    let cbs = defaultConfig { onLoadEnd = \x -> print (responseText x) }
    post cbs "/api/12" {test: 24}
