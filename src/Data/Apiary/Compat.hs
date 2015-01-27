{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}

-- | compatibility module for ghc-7.8 & ghc-7.6.
module Data.Apiary.Compat
    ( -- * type level string literal
      Symbol, KnownSymbol, symbolVal
    , SProxy(..)

      -- * type level natural literal
    , Nat, KnownNat, natVal, type (+)

    -- * Data.Typeables
    , module Data.Typeable
#if !MIN_VERSION_base(4,7,0)
    , typeRep
    , Proxy(..)
#endif
    ) where

import GHC.TypeLits
import Data.Typeable
#if !MIN_VERSION_base(4,7,0)

typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
{-# INLINE typeRep #-}

type KnownSymbol (n :: Symbol) = SingRep n String

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (sing :: Sing n)
{-# INLINE symbolVal #-}

data Proxy (a :: k) = Proxy

type KnownNat (n :: Nat) = SingRep n Integer

natVal :: forall n proxy. KnownNat n => proxy n -> Integer
natVal _ = fromSing (sing :: Sing n)
{-# INLINE natVal #-}
#endif

-- | Symbol Proxy for ghc-7.6 Template Haskell.
data SProxy (a :: Symbol) = SProxy
