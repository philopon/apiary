{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Apiary.Extension
    ( Has(getExtension)
    , Extensions
    , addExtension
    , Initializer, Initializer'
    , initializer, initializerBracket
    , preAction, (+>)
    , noExtension
    ) where

import Data.Apiary.Extension.Internal

type Initializer' m a = forall i. Initializer m i (a ': i)

addExtension :: e -> Extensions es -> Extensions (e ': es)
addExtension = AddExtension

initializer :: Monad m => m e -> Initializer' m e
initializer m = Initializer $ \es n -> do
    e <- m
    n (addExtension e es)

initializerBracket :: (forall a. (e -> m a) -> m a) -> Initializer m es (e ': es)
initializerBracket b = Initializer $ \es n ->
    b $ \e -> n (addExtension e es)

{-# DEPRECATED preAction "DEPRECATED" #-}
preAction :: Monad m => m a -> Initializer m i i
preAction f = Initializer $ \es n -> f >> n es

(+>) :: Monad m => Initializer m i x -> Initializer m x o -> Initializer m i o
Initializer a +> Initializer b = Initializer $ \e m -> a e (\e' -> b e' m)

noExtension :: Monad m => Initializer m '[] '[]
noExtension = Initializer $ \_ n -> n NoExtension
