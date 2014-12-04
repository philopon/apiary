{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Web.Apiary.Develop {-# WARNING "use Web.Apiary in production." #-}
    ( file, file'
    , module Web.Apiary
    ) where

import Web.Apiary hiding (file, file')
import Control.Monad.Apiary.Action (devFile, devFile')

-- | send file contents as lazy bytestring response with detect
-- Content-Type. since v1.2.0.
file :: MonadIO m => FilePath -> Maybe FilePart -> ActionT exts prms m ()
file f _ = devFile f

-- | send file contents as lazy bytestring response. since v1.2.0.
file' :: MonadIO m => FilePath -> Maybe FilePart -> ActionT exts prms m ()
file' f _ = devFile' f
