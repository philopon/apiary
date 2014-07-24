{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Web.Apiary
import Web.Apiary.PureScript
import Network.Wai.Handler.Warp
import Language.Haskell.TH
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as L

-- please run
-- $ bower install philopon/purescript-xhr
-- in examples directory.

main :: IO ()
main = do
    setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)

    withPureScript def {development = True} $ run 3000 . runApiary def $ do
        method GET $ do
            root . action $ do
                contentType "text/html"
                file "pureScript.html" Nothing

            [capture|/test.js|] . action $
                pureScript "pureScript.purs"

        [capture|/api/:Int|] .
            ("test" ?? "b(default: 0)" =?: pInt) . 
            document "a + b" . action $ \i mbj -> do
                let j = maybe 0 id mbj
                contentType "application/json"
                lbs . L.pack $ "{ \"test\": " ++ show (i + j) ++ "}"

        [capture|/api/documentation|] . method GET . action $
            defaultDocumentationAction def
