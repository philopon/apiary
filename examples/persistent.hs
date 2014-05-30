{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

import Web.Apiary
import Web.Apiary.Database.Persist
import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Database.Persist.TH
import Web.Apiary.Logger
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Number 
    number Int
    deriving Show
|]

main :: IO ()
-- give logging setting.
main = withLogger def $ withLogger' def {bufferSize = 0 } $ \immediate -> 

    -- give sql setting.
    withWithSqlPool (withSqlitePool ":memory:" 10) $ do

        -- logging by Given logging setting from 'withLogger'. 
        -- so logging stderr(buffered).
        -- you can change this behaviour by first argument of withLogger.
        runGivenLoggerT . runSql $ runMigration migrateAll

        run 3000 . runApiary def $ do
            root . sql (selectList ([] :: [Filter Number]) []) . action $ \q -> do

                -- logging.
                logging "root accessed.\n"

                lbs $ L.pack (show q)

            [capture|/:Int|] $ do
                stdMethod GET . action $ \i -> do

                    -- if you want to do local logging action.
                    -- logging stdout immediately.
                    c <- runStdoutLoggingT . runSql $ count [NumberNumber ==. i]
                    lbs $ L.pack (show c)

                stdMethod POST . action $ \i -> do
                    _ <- immediate $ runSql $ insert (Number i)
                    return ()

                stdMethod DELETE . action $ \i -> do
                    runSql $ deleteWhere [NumberNumber ==. i]
