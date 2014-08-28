{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Apiary.Extension
    ( Extension
    , Extensions
    , noExtension
    , addExtension
    ) where

import Data.Apiary.SList
import Data.Apiary.Extension.Internal

class Extension a

noExtension :: Extensions '[]
noExtension = Extensions SNil

addExtension :: Extension e => e -> Extensions es -> Extensions (e ': es)
addExtension e (Extensions es) = Extensions $ e ::: es
