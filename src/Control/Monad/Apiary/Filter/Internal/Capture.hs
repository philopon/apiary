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

import Data.Apiary.Compat
import Data.Apiary.Param
import Data.Apiary.Dict
import Data.Apiary.Document

import qualified Data.Text as T
import Text.Blaze.Html

-- | check first path and drill down. since 0.11.0.
path :: Monad actM => T.Text -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
path p = focus' (DocPath p) Nothing (Exact p:) getParams

-- | check consumed paths. since 0.11.1.
endPath :: (Monad actM) => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
endPath = focus' id Nothing (EndPath:) getParams

-- | get first path and drill down. since 0.11.0.
fetch :: (NotMember k prms, KnownSymbol k, Path p, Monad actM) => proxy k -> proxy' p -> Maybe Html
      -> ApiaryT exts (k := p ': prms) actM m () -> ApiaryT exts prms actM m ()
fetch k p h = focus' (DocFetch (T.pack $ symbolVal k) (pathRep p) h) Nothing (FetchPath:) $ liftM actionFetches getState >>= \case
    []   -> mzero
    f:fs -> case readPathAs p f of
        Just r  -> do
            modifyState (\s -> s {actionFetches = fs})
            insert k r <$> getParams
        Nothing -> mzero

anyPath :: (Monad m, Monad actM) => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
anyPath = focus' DocAny Nothing (RestPath:) getParams

restPath :: (NotMember k prms, KnownSymbol k, Monad m, Monad actM)
         => proxy k -> Maybe Html -> ApiaryT exts (k := [T.Text] ': prms) actM m () -> ApiaryT exts prms actM m ()
restPath k h = focus' (DocRest (T.pack $ symbolVal k) h) Nothing (RestPath:) $ getParams >>= \l -> liftM actionFetches getState >>= \case
    [] -> return $ insert k [] l
    fs -> insert k fs l <$ modifyState (\s -> s {actionFetches = []})
