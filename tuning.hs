{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Network.Wai.Handler.Warp

main :: IO ()
main = server (run 3000) . runApiary def $ do
    method GET . action $ do
        bytes "test"
