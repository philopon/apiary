{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Web.Apiary
import Web.Apiary.PureScript
import Network.Wai.Handler.Warp
import Language.Haskell.TH
import System.Directory
import System.FilePath

-- please run
-- $ bower install purescript-foldable-traversable purescript-arrays
-- in examples directory.

main :: IO ()
main = do
    setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)
    withPureScript def $ run 3000 . runApiary def $ do
        method GET $ do
            root . action $ do
                contentType "text/html"
                file "pureScript.html" Nothing

            [capture|/test.js|] . method GET . action $
                pureScript "pureScript.purs"
