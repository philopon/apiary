{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.PureScript
    ( I.PureScriptConfig(..)
    , initPureScript
    , pureScript
    ) where

import Web.Apiary
import qualified Web.Apiary.PureScript.Internal as I
import Data.Apiary.Extension
import Data.Apiary.Proxy

initPureScript :: MonadIO m => I.PureScriptConfig -> Initializer' m I.PureScript
initPureScript = initializer . I.makePureScript

pureScript :: (Has I.PureScript exts, MonadIO m) => FilePath -> ActionT exts m ()
pureScript m = getExt (Proxy :: Proxy I.PureScript) >>= flip I.pureScript m
