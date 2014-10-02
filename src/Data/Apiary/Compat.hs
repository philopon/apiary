{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Apiary.Compat
    ( module Export
    , Symbol, KnownSymbol, symbolVal
    , SProxy(..)
#if __GLASGOW_HASKELL__ < 707
    , typeRep
#endif
    ) where

import GHC.TypeLits
#if __GLASGOW_HASKELL__ > 707
import Data.Typeable as Export
#else
import Data.Proxy    as Export
import Data.Typeable as Export
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
{-# INLINE typeRep #-}

type KnownSymbol (n :: Symbol) = SingRep n String

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (sing :: Sing n)
#endif

data SProxy (a :: Symbol) = SProxy

