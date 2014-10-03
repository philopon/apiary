{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | compatibility module for ghc-7.8 & ghc-7.6.
module Data.Apiary.Compat
    ( -- * type level string literal
      Symbol, KnownSymbol, symbolVal
    , SProxy(..)

    -- * Data.Typeables
#if MIN_VERSION_base(4,7,0)
    , module Data.Typeable
#else
    , typeRep
    , module Data.Proxy
    , module Data.Typeable
#endif
    ) where

import GHC.TypeLits
#if MIN_VERSION_base(4,7,0)
import Data.Typeable
#else
import Data.Proxy
import Data.Typeable

typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
{-# INLINE typeRep #-}

type KnownSymbol (n :: Symbol) = SingRep n String

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (sing :: Sing n)
#endif

-- | Symbol Proxy for ghc-7.6.
data SProxy (a :: Symbol) = SProxy
