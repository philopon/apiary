{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Apiary.Filter.Internal.Capture where

import Control.Applicative((<$>), (<$))
import Control.Monad(liftM, mzero)
import Control.Monad.Apiary.Action.Internal
    (getParams, actionFetches, getState, modifyState)
import Control.Monad.Apiary.Internal
    (ApiaryT, focus', PathElem(Exact, FetchPath, RestPath, EndPath))
import Control.Monad.Apiary.Filter.Internal
    (Doc(DocPath, DocFetch, DocAny, DocRest))

import Data.Apiary.Compat(KnownSymbol, symbolVal, Proxy(..))
import Data.Apiary.Param(Path, pathRep, readPathAs)
import Data.Apiary.Dict(Elem((:=)))
import qualified Data.Apiary.Dict as Dict

import qualified Data.Text as T
import Text.Blaze.Html(Html)

-- | check first path and drill down. since 0.11.0.
path :: Monad actM => T.Text -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
path p = focus' (DocPath p) Nothing (Exact p:) getParams

-- | check consumed paths. since 0.11.1.
endPath :: (Monad actM) => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
endPath = focus' id Nothing (EndPath:) getParams

-- | get first path and drill down. since 0.11.0.
fetch' :: (Dict.NotMember k prms, KnownSymbol k, Path p, Monad actM) => proxy k -> proxy' p -> Maybe Html
       -> ApiaryT exts (k := p ': prms) actM m () -> ApiaryT exts prms actM m ()
fetch' k p h = focus' (DocFetch (T.pack $ symbolVal k) (pathRep p) h) Nothing (FetchPath:) $ liftM actionFetches getState >>= \case
    []   -> mzero
    f:fs -> case readPathAs p f of
        Just r  -> do
            modifyState (\s -> s {actionFetches = fs})
            Dict.insert k r <$> getParams
        Nothing -> mzero


fetch :: forall proxy k p exts prms actM m. (Dict.NotMember k prms, KnownSymbol k, Path p, Monad actM)
      => proxy (k := p) -> Maybe Html
      -> ApiaryT exts (k := p ': prms) actM m () -> ApiaryT exts prms actM m ()
fetch _ h = fetch' k p h
  where
    k = Proxy :: Proxy k
    p = Proxy :: Proxy p

anyPath :: (Monad m, Monad actM) => ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
anyPath = focus' DocAny Nothing (RestPath:) getParams

restPath :: (Dict.NotMember k prms, KnownSymbol k, Monad m, Monad actM)
         => proxy k -> Maybe Html -> ApiaryT exts (k := [T.Text] ': prms) actM m () -> ApiaryT exts prms actM m ()
restPath k h = focus' (DocRest (T.pack $ symbolVal k) h) Nothing (RestPath:) $ getParams >>= \l -> liftM actionFetches getState >>= \case
    [] -> return $ Dict.insert k [] l
    fs -> Dict.insert k fs l <$ modifyState (\s -> s {actionFetches = []})
