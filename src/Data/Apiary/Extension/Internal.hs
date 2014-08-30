{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Apiary.Extension.Internal where

import Control.Category

newtype Initializer m i o = Initializer 
    {unInitializer :: Extensions i -> m (Extensions o)}

instance Monad m => Category (Initializer m) where
    id    = Initializer return
    Initializer a . Initializer b = Initializer $ \e -> b e >>= a

data Extensions (es :: [*]) where
    NoExtension  :: Extensions '[]
    AddExtension :: (e :: *) -> Extensions es -> Extensions (e ': es)

class Has a (as :: [*]) where
    getExtension :: proxy a -> Extensions as -> a

instance Has a (a ': as) where
    getExtension _ (AddExtension a _) = a

instance Has a as => Has a (a' ': as) where
    getExtension p (AddExtension _ as) = getExtension p as

