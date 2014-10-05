{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Web.Apiary
import Web.Apiary.Database.Persist
import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Database.Persist.TH
import Web.Apiary.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Number 
    number Int
    deriving Show
|]

main :: IO ()
--                               logger extension   persist extension
main = runApiaryWith (run 3000) (initLogger def  +> initPersistPool (withSqlitePool "db.sqlite" 10) migrateAll) def $ do
--                                               ~~
--                                               compose 2 extension intializer.

    -- root : list up all database entities.
    root . sql Nothing  [key|query|]  (selectList ([] :: [Filter Number]) [])   Just . action $ do
    --         ~~~~~~~  ~~~~~~~~~~~~  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   ~~~~
    --         docuemnt param key     persist selector                          check filter success or not.

        -- logging.
        logging "root accessed.\n"
        mapM_ (\a -> showing a >> char '\n' ) =<< param [key|query|]

    -- /:Int : numberd counter
    [capture|/i::Int|] $ do

        -- GET: get counter.
        method GET . action $ do
            i <- param [key|i|]

            -- execute persistent
            c <- runSql $ count [NumberNumber ==. i]
            showing c
            char '\n'

        -- POST: increment counter.
        method POST . action $ do
            i <- param [key|i|]
            runSql $ insert_ (Number i)

        -- DELETE: reset counter.
        method DELETE . action $ do
            i <- param [key|i|]
            runSql $ deleteWhere [NumberNumber ==. i]

{-
$ curl -XGET  localhost:3000
$ curl -XGET  localhost:3000/1
0
$ curl -XPOST localhost:3000/1
$ curl -XGET  localhost:3000/1
1
$ curl -XPOST localhost:3000/2
$ curl -XPOST localhost:3000/2
$ curl -XGET  localhost:3000
Entity {entityKey = NumberKey {unNumberKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = Number {numberNumber = 1}}
Entity {entityKey = NumberKey {unNumberKey = SqlBackendKey {unSqlBackendKey = 2}}, entityVal = Number {numberNumber = 2}}
Entity {entityKey = NumberKey {unNumberKey = SqlBackendKey {unSqlBackendKey = 3}}, entityVal = Number {numberNumber = 2}}
$ curl -XDELETE localhost:3000/2
$ curl -XGET  localhost:3000
Entity {entityKey = NumberKey {unNumberKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = Number {numberNumber = 1}}
-}
