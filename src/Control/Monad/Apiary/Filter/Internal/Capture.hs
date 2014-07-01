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

import Control.Monad.Apiary.Action.Internal
import Control.Monad.Apiary
import Control.Monad.Apiary.Filter.Internal

-- | check first path and drill down. since v0.11.0.
path :: (Functor n, Monad n) 
     => T.Text -> ApiaryT c n m a -> ApiaryT c n m a
path p = focus $ \l -> path' >> return l
  where
    path' = liftM actionPathInfo getState >>= \case
        c:_ | c == p -> modifyState (\s -> s {actionPathInfo = tail $ actionPathInfo s})
        _            -> empty

-- | get first path and drill down. since v0.11.0.
fetch :: (Path a, Functor n, Monad n)
      => proxy a -> ApiaryT (Snoc as a) n m b -> ApiaryT as n m b
fetch p = focus $ \l -> liftM actionPathInfo getState >>= \case
    []  -> empty
    c:_ -> case readPathAs p c of
        Nothing -> empty
        Just r  -> do
            modifyState (\s -> s { actionPathInfo = tail $ actionPathInfo s})
            return (sSnoc l r)
