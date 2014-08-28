{-# LANGUAGE DataKinds #-}

module Data.Apiary.Extension.Internal where

import Data.Apiary.SList

newtype Extensions exts = Extensions { unExtensions :: SList exts }

