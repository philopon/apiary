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
path :: Monad actM => T.Text -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
path p = focus' (DocPath p) Nothing (Exact p:) return

-- | check consumed paths. since 0.11.1.
endPath :: (Monad actM) => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
endPath = focus' id Nothing (EndPath:) return

-- | get first path and drill down. since 0.11.0.
fetch :: (Path p, Monad actM) => proxy p -> Maybe Html
      -> ApiaryT exts (p ': prms) actM m () -> ApiaryT exts prms actM m ()
fetch p h = focus' (DocFetch (pathRep p) h) Nothing (FetchPath:) $ \l -> liftM actionFetches getState >>= \case
    []   -> mzero
    f:fs -> case readPathAs p f of
        Just r  -> (r ::: l) <$ modifyState (\s -> s {actionFetches = fs})
        Nothing -> mzero

restPath :: (Monad m, Monad actM) => ApiaryT exts ([T.Text] ': prms) actM m () -> ApiaryT exts prms actM m ()
restPath = focus' id Nothing (RestPath:) $ \l -> liftM actionFetches getState >>= \case
    [] -> return $ [] ::: l
    fs -> fs ::: l <$ modifyState (\s -> s {actionFetches = []})
