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
import qualified Data.ByteString.Lazy.Char8 as L

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Number 
    number Int
    deriving Show
|]

main :: IO ()
main = withWithSqlPool (withSqlitePool ":memory:" 10) $ do
    runSql $ runMigration migrateAll
    run 3000 . runApiary def $ do
        root . action $ do
            l <- runSql $ selectList ([] :: [Filter Number]) []
            lbs $ L.pack (show l)

        [capture|/:Int|] $ do
            stdMethod GET . action $ \i -> do
                c <- runSql $ count [NumberNumber ==. i]
                lbs $ L.pack (show c)

            stdMethod POST . action $ \i -> do
                _ <- runSql $ insert (Number i)
                return ()

            stdMethod DELETE . action $ \i -> do
                runSql $ deleteWhere [NumberNumber ==. i]
