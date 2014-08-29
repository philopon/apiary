{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Data.Apiary.Extension
    ( Has(getExtension)
    , Extension
    , Extensions
    , addExtension
    , Initializer(..)
    , initializer, preAction, (+>)
    , hoistInitializer
    , noExtension
    ) where

import Data.Apiary.Extension.Internal

addExtension :: Extension e => e -> Extensions es -> Extensions (e ': es)
addExtension = AddExtension

initializer :: (Extensions i -> m (Extensions (e ': i))) -> Initializer i m (e ': i)
initializer = Initializer

preAction :: Monad m => m a -> Initializer i m i
preAction f = Initializer $ \e -> f >> return e

(+>) :: Monad m => Initializer i m x -> Initializer x m o -> Initializer i m o
Initializer a +> Initializer b = Initializer $ \e -> a e >>= b

hoistInitializer :: (forall b. m b -> n b) -> Initializer i m a -> Initializer i n a
hoistInitializer r (Initializer i) = Initializer $ \e -> r (i e)

noExtension :: Monad m => Initializer '[] m '[]
noExtension = Initializer $ return
