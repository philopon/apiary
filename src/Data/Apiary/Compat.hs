{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Apiary.Compat
    ( module Export
    , Symbol, KnownSymbol, symbolVal
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

type KnownSymbol n = SingRep n String

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (sing :: Sing n)
#endif
