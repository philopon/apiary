{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}

module Control.Monad.Apiary.Filter.Internal.Capture where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Data.Apiary.Param
import Data.Apiary.SList
import Data.Apiary.Document

import Control.Monad.Apiary.Action.Internal
import Control.Monad.Apiary.Internal

-- | check first path and drill down. since v0.11.0.
path :: Monad n => T.Text -> ApiaryT c n m a -> ApiaryT c n m a
path p = focus (DocPath p) $ \l -> l <$ path'
  where
    path' = liftM actionPathInfo getState >>= \case
        c:_ | c == p -> modifyState (\s -> s {actionPathInfo = tail $ actionPathInfo s})
        _            -> mzero

-- | check consumed pathes. since v0.11.1.
endPath :: Monad n => ApiaryT c n m a -> ApiaryT c n m a
endPath = focus id $ \l -> l <$ end
  where
    end = liftM actionPathInfo getState >>= \case
        [] -> return ()
        _  -> mzero

-- | get first path and drill down. since v0.11.0.
fetch :: (Path a, Monad n)
      => proxy a -> ApiaryT (Snoc as a) n m b -> ApiaryT as n m b
fetch p = focus (DocFetch $ pathRep p) $ \l -> liftM actionPathInfo getState >>= \case
    []  -> mzero
    c:_ -> case readPathAs p c of
        Nothing -> mzero
        Just r  -> do
            modifyState (\s -> s { actionPathInfo = tail $ actionPathInfo s})
            return (sSnoc l r)
