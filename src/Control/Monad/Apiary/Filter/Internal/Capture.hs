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
import Control.Monad.Apiary.Action.Internal
import Control.Monad.Apiary.Internal

import Data.Apiary.Param
import Data.Apiary.SList
import Data.Apiary.Document

import qualified Data.Text as T
import Text.Blaze.Html

-- | check first path and drill down. since 0.11.0.
path :: Monad n => T.Text -> ApiaryT c n m a -> ApiaryT c n m a
path p = focus (DocPath p) Nothing (Exact p:) return

-- | check consumed paths. since 0.11.1.
endPath :: (Functor n, Monad n) => ApiaryT c n m a -> ApiaryT c n m a
endPath = focus id Nothing (EndPath:) return

-- | get first path and drill down. since 0.11.0.
fetch :: (Path a, Functor n, Monad n) => proxy a -> Maybe Html -> ApiaryT (Snoc as a) n m b -> ApiaryT as n m b
fetch p h = focus (DocFetch (pathRep p) h) Nothing (FetchPath:) $ \l -> liftM actionFetches getState >>= \case
    []   -> mzero
    f:fs -> case readPathAs p f of
        Nothing -> mzero
        Just r  -> sSnoc l r <$ modifyState (\s -> s {actionFetches = fs})
