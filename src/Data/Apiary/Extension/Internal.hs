{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Apiary.Extension.Internal where

newtype Initializer i m o = Initializer 
    {unInitializer :: Extensions i -> m (Extensions o)}

class Extension a

data Extensions (es :: [*]) where
    NoExtension  :: Extensions '[]
    AddExtension :: Extension e => (e :: *) -> Extensions es -> Extensions (e ': es)

class Has a (as :: [*]) where
    getExtension :: proxy a -> Extensions as -> a

instance Has a (a ': as) where
    getExtension _ (AddExtension a _) = a

instance Has a as => Has a (a' ': as) where
    getExtension p (AddExtension _ as) = getExtension p as

