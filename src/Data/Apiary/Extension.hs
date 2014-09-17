{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Apiary.Extension
    ( Has(getExtension)
    , Extensions
    , addExtension
    , Initializer,  initializer
    , Initializer', initializer'
    , initializerBracket
    , initializerBracket'
    , preAction, (+>)
    , noExtension
    ) where

import Data.Apiary.Extension.Internal

type Initializer' m a = forall i. Initializer m i (a ': i)

addExtension :: e -> Extensions es -> Extensions (e ': es)
addExtension = AddExtension

initializer :: Monad m => (Extensions es -> m e) -> Initializer m es (e ': es)
initializer m = Initializer $ \es n -> do
    e <- m es
    n (addExtension e es)

initializer' :: Monad m => m e -> Initializer' m e
initializer' m = initializer (const m)

initializerBracket :: (forall a. Extensions es -> (e -> m a) -> m a) -> Initializer m es (e ': es)
initializerBracket b = Initializer $ \es n ->
    b es $ \e -> n (addExtension e es)

initializerBracket' :: (forall a. (e -> m a) -> m a) -> Initializer m es (e ': es)
initializerBracket' m = initializerBracket (const m)

{-# DEPRECATED preAction "DEPRECATED" #-}
preAction :: Monad m => m a -> Initializer m i i
preAction f = Initializer $ \es n -> f >> n es

(+>) :: Monad m => Initializer m i x -> Initializer m x o -> Initializer m i o
Initializer a +> Initializer b = Initializer $ \e m -> a e (\e' -> b e' m)

noExtension :: Monad m => Initializer m i i
noExtension = Initializer $ \es n -> n es
