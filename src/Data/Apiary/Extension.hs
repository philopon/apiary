{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}

module Data.Apiary.Extension
    ( Has(getExtension)
    , MonadExts(..), getExt
    , Middleware'
    , Extension(..)
    , Extensions
    , noExtension
    -- * initializer constructor
    , Initializer,  initializer
    , Initializer', initializer'
    , initializerBracket
    , initializerBracket'

    -- * combine initializer
    , (+>)
    ) where

import Control.Monad(liftM)
import Data.Apiary.Extension.Internal(Has, getExtension, Initializer(Initializer))
import Control.Monad.Apiary.Action.Internal
    (MonadExts(getExts), Extension(..), Extensions(AddExtension), Middleware')

getExt :: (MonadExts es m, Has e es) => proxy e -> m e
getExt p = getExtension p `liftM` getExts

type Initializer' m a = forall i. Initializer m i (a ': i)

addExtension :: Extension e => e -> Extensions es -> Extensions (e ': es)
addExtension = AddExtension

initializer :: (Extension e, Monad m) => (Extensions es -> m e) -> Initializer m es (e ': es)
initializer m = Initializer $ \es n -> do
    e <- m es
    n (addExtension e es)

initializer' :: (Extension e, Monad m) => m e -> Initializer' m e
initializer' m = initializer (const m)

initializerBracket :: Extension e => (forall a. Extensions es -> (e -> m a) -> m a) -> Initializer m es (e ': es)
initializerBracket b = Initializer $ \es n ->
    b es $ \e -> n (addExtension e es)

initializerBracket' :: Extension e => (forall a. (e -> m a) -> m a) -> Initializer m es (e ': es)
initializerBracket' m = initializerBracket (const m)

-- | combine two Initializer. since 0.16.0.
(+>) :: Monad m => Initializer m i x -> Initializer m x o -> Initializer m i o
Initializer a +> Initializer b = Initializer $ \e m -> a e (\e' -> b e' m)

noExtension :: Monad m => Initializer m i i
noExtension = Initializer $ \es n -> n es
