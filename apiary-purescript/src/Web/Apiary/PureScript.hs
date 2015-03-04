{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Web.Apiary.PureScript
    ( I.PureScript
    , I.PureScriptConfig(..)
    , initPureScript
    , pureScript
    ) where

import Web.Apiary(MonadIO(..))
import Control.Monad.Apiary.Action(ActionT)
import qualified Web.Apiary.PureScript.Internal as I
import Data.Apiary.Extension(Initializer', initializer', Has, getExt)
import Data.Proxy.Compat(Proxy(..))

initPureScript :: MonadIO m => I.PureScriptConfig -> Initializer' m I.PureScript
initPureScript = initializer' . liftIO . I.makePureScript

pureScript :: (Has I.PureScript exts, MonadIO m) => FilePath -> ActionT exts prms m ()
pureScript m = getExt (Proxy :: Proxy I.PureScript) >>= flip I.pureScript m
