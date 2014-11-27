{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

module Data.Apiary.Extension.Internal where

#if __GLASGOW_HASKELL__ >= 708
import qualified Control.Category as Cat
#endif
import Network.Wai
import Control.Monad.Apiary.Action.Internal

class Has a (as :: [*]) where
    getExtension :: proxy a -> Extensions as -> a

instance Has a (a ': as) where
    getExtension _ (AddExtension a _) = a

instance Has a as => Has a (a' ': as) where
    getExtension p (AddExtension _ as) = getExtension p as

newtype Initializer m i o = Initializer 
    {unInitializer :: forall a. Extensions i -> (Extensions o -> m a) -> m a}

allMiddleware' :: Extensions es -> Middleware'
allMiddleware' NoExtension         = id
allMiddleware' (AddExtension e es) = extMiddleware' e . allMiddleware' es

allMiddleware :: Extensions es -> Middleware
allMiddleware NoExtension = id
allMiddleware (AddExtension e es) = extMiddleware e . allMiddleware es

#if __GLASGOW_HASKELL__ >= 708
instance Monad m => Cat.Category (Initializer m) where
    id = Initializer $ \es m -> m es
    Initializer a . Initializer b = Initializer $ \e m -> b e (\e' -> a e' m)
#endif
