{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Web.Apiary
import Web.Apiary.PureScript
import Network.Wai.Handler.Warp
import System.Directory
import System.Environment
import System.FilePath

-- please run
-- $ bower install purescript-foldable-traversable purescript-arrays
-- in examples directory.

main :: IO ()
main = do
    setCurrentDirectory (takeDirectory __FILE__)
    withPureScript def $ run 3000 . runApiary def $ do
        method GET $ do
            root . action $ do
                contentType "text/html"
                file "pureScript.html" Nothing

            [capture|/test.js|] . method GET . action $
                pureScript "pureScript.purs"
