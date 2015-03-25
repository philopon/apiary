{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Apiary.Extension.Internal
    ( Initializer(..)
    , Has(..)
    , allMiddleware'
    , allMiddleware
    ) where

#if __GLASGOW_HASKELL__ >= 708
import qualified Control.Category as Cat
#endif
import qualified Network.Wai as Wai
import Control.Monad.Apiary.Action.Internal
    ( Extensions(AddExtension, NoExtension)
    , Extension(extMiddleware, extMiddleware'), Middleware')

class Has a (as :: [*]) where
    getExtension :: proxy a -> Extensions as -> a

instance Has a (a ': as) where
    getExtension _ (AddExtension a _) = a

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPABLE #-} Has a as => Has a (a' ': as) where
#else
instance Has a as => Has a (a' ': as) where
#endif
    getExtension p (AddExtension _ as) = getExtension p as

newtype Initializer m i o = Initializer 
    {unInitializer :: forall a. Extensions i -> (Extensions o -> m a) -> m a}

allMiddleware' :: Extensions es -> Middleware'
allMiddleware' NoExtension         = id
allMiddleware' (AddExtension e es) = extMiddleware' e . allMiddleware' es

allMiddleware :: Extensions es -> Wai.Middleware
allMiddleware NoExtension = id
allMiddleware (AddExtension e es) = extMiddleware e . allMiddleware es

#if __GLASGOW_HASKELL__ >= 708
instance Monad m => Cat.Category (Initializer m) where
    id = Initializer $ \es m -> m es
    Initializer a . Initializer b = Initializer $ \e m -> b e (\e' -> a e' m)
#endif
