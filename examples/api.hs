{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Web.Apiary
import Network.Wai.Handler.Warp
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Control.Monad
import Control.Concurrent
import Data.Monoid

data Test = Test 
    { name :: Maybe String
    , age  :: Int
    } deriving Show

deriveToJSON defaultOptions ''Test

-- You can view API documentation on 'http://localhost:3000/api/documentation'.

main :: IO ()
main = do
    nm <- newMVar Nothing
    ag <- newMVar 0

    runApiary (run 3000) def $ do

        -- you can add route document using document function.
        -- condition which is next of noDoc function is not documented.
        [capture|/precondition|] .
            http11 .
            accept "text/plain" .
            precondition (preEscaped "user <strong>defined</strong> precondition.") .
            noDoc . header [key|User-Agent|] . -- <- hasHeader "User-Agent" is not documented because it is next of noDoc.
            document "precondition test" .  action $
                bytes "precondition"


        -- you can add document group only as top level action.
        group "cat group" $ do
            [capture|/api/cat|] $ do
                method GET . document "get current name and age." . action $ do
                    n <- liftIO $ readMVar nm
                    a <- liftIO $ readMVar ag
                    contentType "application/json"
                    lazyBytes $ JSON.encode $ Test n a

                method POST .
                    -- you can add query description using (??) function.
                    ([key|name|]         =:  pMaybe pString) .
                    ([key|age|] ?? dAge  =?: pInt) .
                    document "set name and age from query parameter." . action $ do
                        n <- param [key|name|]
                        a <- param [key|age|]
                        liftIO . void $ swapMVar nm n
                        liftIO $ maybe (return ()) (void . swapMVar ag) a

                -- when document function not exists, it be undocumented route.
                method DELETE . action $ do
                    void . liftIO $ swapMVar nm Nothing
                    void . liftIO $ swapMVar ag 0

                method CONNECT . document "doc" . action $ return ()

            -- you can add route capture description using [].
            -- you can reference value using '$'.
            [capture|/api/cat/name::String[$dName]/age::Int[age]|] .
                method POST . document "set name and age from route." . action $ do
                    n <- param [key|name|]
                    a <- param [key|age|]
                    void . liftIO $ swapMVar nm (Just n)
                    void . liftIO $ swapMVar ag a

        -- no document function -> hidden route.
        root . method GET . action $ do
            contentType "text/html"
            bytes "<h1>root</h1><a href=\"/api/documentation\">api document</a>"

        -- you can generate API document with multiple action.
        -- rpHtml function format as captured route parameter.
        group "dog group" $ do
            [capture|/api/dog/i::Int/**rest|] $ do
                precondition ("i is even.") . document "twice" . action $ do
                    i <- param [key|i|]
                    guard $ even i
                    contentType "text/plain"
                    showing (i * 2)
                precondition ("i is odd.") . document "succ" . action $ do
                    i <- param [key|i|]
                    contentType "text/plain"
                    showing (succ i)

        -- add documentation page route.
        [capture|/api/documentation|] . document "this page" $ documentation def
            { documentTitle       = "Example of API documentation auto generation"
            , documentDescription = Just $ preEscaped
                "source file: <a href=\"https://github.com/philopon/apiary/blob/master/examples/api.hs\">here</a>"
            }

        [capture|/neko|] . document "nyan" $ do
            accept "text/plain"       . action $ bytes "nyan"
            accept "application/json" . action $ bytes "{\"neko\": \"yes\"}"

        [capture|/static/api-documentation.js|] .document "doc" . action $ file "static/api-documentation.js" Nothing
        [capture|/static/api-documentation.css|] . action $ file "static/api-documentation.css" Nothing

dName :: Html
dName = preEscaped "<ul><li>nコンビニame of cat</li><li>if null, homeless</li></ul>"

dAge :: Html
dAge = preEscaped "<ul><li>遭遇age of cat.</li><li>cute!</li></ul>"
