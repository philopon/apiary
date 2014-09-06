{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

module Data.Apiary.Extension.Internal where

import Control.Category

data Extensions (es :: [*]) where
    NoExtension  :: Extensions '[]
    AddExtension :: (e :: *) -> Extensions es -> Extensions (e ': es)

class Has a (as :: [*]) where
    getExtension :: proxy a -> Extensions as -> a

instance Has a (a ': as) where
    getExtension _ (AddExtension a _) = a

instance Has a as => Has a (a' ': as) where
    getExtension p (AddExtension _ as) = getExtension p as

newtype Initializer m i o = Initializer 
    {unInitializer :: forall a. Extensions i -> (Extensions o -> m a) -> m a}

#if __GLASGOW_HASKELL__ >= 708
instance Monad m => Category (Initializer m) where
    id = Initializer $ \es m -> m es
    Initializer a . Initializer b = Initializer $ \e m -> b e (\e' -> a e' m)
#endif
