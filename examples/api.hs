{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Network.Wai.Handler.Warp
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Control.Monad
import Control.Concurrent
import Text.Blaze.Html
import Data.Monoid
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Apiary.Document.Html

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
            precondition ("user " <> H.strong "defined" <> " precondition.") .
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
            bytes "root"

        -- you can generate API document with multiple action.
        -- rpHtml function format as captured route parameter.
        group "dog group" $ do
            [capture|/api/dog/i::Int/**rest|] $ do
                precondition (rpHtml "i" Nothing <> " is even.") . document "twice" . action $ do
                    i <- param [key|i|]
                    guard $ even i
                    contentType "text/plain"
                    showing (i * 2)
                precondition (rpHtml "i" Nothing <> " is odd.") . document "succ" . action $ do
                    i <- param [key|i|]
                    contentType "text/plain"
                    showing (succ i)

        -- add documentation page route.
        [capture|/api/documentation|] . method GET . document "this page" . action $
            defaultDocumentationAction def 
                { documentTitle       = "Example of API documentation auto generation"
                , documentDescription = Just $ H.p $ mconcat
                    [ "source file: "
                    , H.a ! A.href "https://github.com/philopon/apiary/blob/master/examples/api.hs" $ "here"
                    ]
                }

dName :: Html
dName = H.ul $ H.li "name of cat." <> H.li "if null, homeless."

dAge :: Html
dAge = H.ul $ H.li "age of cat." <> H.li "cute!"
