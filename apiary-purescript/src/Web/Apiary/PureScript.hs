{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.PureScript
    ( I.PureScriptConfig(..)
    , HasPureScript
    , withPureScript
    , pureScript
    ) where

import Web.Apiary
import qualified Web.Apiary.PureScript.Internal as I
import Data.Reflection

type HasPureScript = Given I.PureScript

withPureScript :: MonadIO m => I.PureScriptConfig
               -> (HasPureScript => m a) -> m a
withPureScript conf m = I.withPureScript conf $ \p -> give p m

pureScript :: (MonadIO m, HasPureScript) => FilePath -> ActionT m ()
pureScript = I.pureScript given
