{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Apiary.Extension
    ( Has(getExtension)
    , Extensions
    , addExtension
    , Initializer, Initializer'
    , initializer, preAction, (+>)
    , noExtension
    ) where

import Data.Apiary.Extension.Internal

type Initializer' m a = forall i. Initializer m i (a ': i)

addExtension :: e -> Extensions es -> Extensions (e ': es)
addExtension = AddExtension

initializer :: Monad m => m e -> Initializer' m e
initializer m = Initializer $ \e -> do
    a <- m
    return (addExtension a e)

{-# DEPRECATED preAction "DEPRECATED" #-}
preAction :: Monad m => m a -> Initializer m i i
preAction f = Initializer $ \e -> f >> return e

{-# DEPRECATED (+>) "use (>>>)" #-}
(+>) :: Monad m => Initializer m i x -> Initializer m x o -> Initializer m i o
Initializer a +> Initializer b = Initializer $ \e -> a e >>= b

noExtension :: Monad m => Initializer m '[] '[]
noExtension = Initializer return
