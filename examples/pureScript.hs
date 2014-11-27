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
-- $ bower install philopon/purescript-xhr
-- in examples directory.

main :: IO ()
main = do
    setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)

    runApiaryWith (run 3000) (initPureScript def { development = True }) def $ do
        method GET $ do
            root . action $ do
                contentType "text/html"
                file "pureScript.html" Nothing

            [capture|/test.js|] . action $
                pureScript "pureScript.purs"

        [capture|/api/i::Int|] .
            ([key|test|] ?? "b(default: 0)" =?: pInt) . 
            document "a + b" . action $ do
                (i, mbj) <- [params|i, test|]
                let j = maybe 0 id mbj
                contentType "application/json"
                bytes "{ \"test\": "
                appendShowing (i + j)
                appendChar '}'

        [capture|/api/documentation|] . method GET . action $
            defaultDocumentationAction def
