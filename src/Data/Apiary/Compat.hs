{-# LANGUAGE UndecidableInstances #-} -- for ghc-7.6
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-} -- for ghc-7.6
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(a,b,c) 1
#endif

-- | compatibility module for ghc-7.8 & ghc-7.6.
module Data.Apiary.Compat
    ( -- * type level string literal
      Symbol, KnownSymbol, symbolVal
    , SProxy(..)

    , Proxy(..)
    , module Data.Typeable
#if MIN_VERSION_base(4,7,0)
    , Typeable.typeRep
#else
    , typeRep
#endif
    ) where

import GHC.TypeLits
import Data.Typeable
    ( Typeable, typeOf, cast, gcast
    , TypeRep, showsTypeRep
    , TyCon, tyConString, tyConPackage, tyConModule, tyConName
    , mkTyCon3, mkTyConApp, mkAppTy, mkFunTy
    , splitTyConApp, funResultTy, typeRepTyCon, typeRepArgs
    )

#if MIN_VERSION_base(4,7,0)
import Data.Proxy(Proxy(..))
import qualified Data.Typeable as Typeable
#else

typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
{-# INLINE typeRep #-}

class SingI n => KnownSymbol (n :: Symbol) where
    symbolSing :: Sing n

instance SingRep n String => KnownSymbol n where
    symbolSing = sing

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (symbolSing :: Sing n)
{-# INLINE symbolVal #-}

data Proxy (a :: k) = Proxy
#endif

-- | Symbol Proxy for ghc-7.6 Template Haskell.
data SProxy (a :: Symbol) = SProxy
