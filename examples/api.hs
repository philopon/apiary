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

data Test = Test 
    { name :: Maybe String
    , age  :: Int
    } deriving Show

deriveToJSON defaultOptions ''Test

-- You can view API documentation on 'http://localhost:3000/api/documentation'.

-- Documentation route, title and description(or all of documentation function)
-- can set in ApiaryConfig.
conf :: ApiaryConfig
conf = def {
    documentationAction = Just $ defaultDocumentationAction "/api/documentation"
        def { documentTitle       = "auto generate API documentation example"
            , documentDescription = Just $ H.p $ mconcat
                [ "example of API documentation. source file: "
                , H.a ! A.href "https://github.com/philopon/apiary/blob/master/examples/api.hs" $ "here"
                ]
            }
    }

main :: IO ()
main = do
    nm <- newMVar Nothing
    ag <- newMVar 0

    run 3000 . runApiary conf $ do

        -- you can add route document using document function.
        root . stdMethod GET . document "root page" . action $ do
            lbs "root"

        -- condition that put after document function, not documented.
        [capture|/precondition|] . http11 . hasHeader "Accept" .
            document "precondition test" . hasHeader "User-Agent" . action $ do
                lbs "precondition"

        -- you can add document group only as top level action.
        group "cat" $ do
            [capture|/api/cat|] $ do
                stdMethod GET . document "get current name and age." . action $ do
                    n <- liftIO $ readMVar nm
                    a <- liftIO $ readMVar ag
                    contentType "application/json"
                    lbs $ JSON.encode $ Test n a

                stdMethod POST .
                    -- you can add query description after ':'.
                    -- when not elem ':', it be undocumented parameter.
                    -- or (??) function to add description.
                    ("name:name of cat" =?: pString) .
                    ("age" ?? dAge      =?: pInt) .
                    document "set name and age from query parameter." . action $ \n a -> do
                        void . liftIO $ swapMVar nm n
                        liftIO $ maybe (return ()) (void . swapMVar ag) a

                -- when document function not exists, it be undocumented route.
                stdMethod DELETE . action $ do
                    void . liftIO $ swapMVar nm Nothing
                    void . liftIO $ swapMVar ag 0

            -- you can add route capture description using ().
            -- you can reference value using '$'.
            [capture|/api/cat/:String($dName)/:Int(age)|] .
                stdMethod POST . document "set name and age from route." . action $ \n a -> do
                    void . liftIO $ swapMVar nm (Just n)
                    void . liftIO $ swapMVar ag a

dName :: Html
dName = H.ul $ H.li "name of cat." <> H.li "if null, homeless."

dAge :: Html
dAge = H.ul $ H.li "age of cat." <> H.li "cute!"
