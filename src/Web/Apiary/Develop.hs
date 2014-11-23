{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Web.Apiary.Develop {-# WARNING "use Web.Apiary in production." #-}
    ( file, file'
    , module Web.Apiary
    ) where

import Web.Apiary hiding (file, file')
import Control.Monad.Apiary.Action (devFile, devFile')

file :: MonadIO m => FilePath -> Maybe FilePart -> ActionT exts prms m ()
file f _ = devFile f

file' :: MonadIO m => FilePath -> Maybe FilePart -> ActionT exts prms m ()
file' f _ = devFile' f
