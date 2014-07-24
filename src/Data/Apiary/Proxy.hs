{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Apiary.Proxy
    ( module Export
#if __GLASGOW_HASKELL__ < 707
    , typeRep
#endif
    ) where

#if __GLASGOW_HASKELL__ > 707
import Data.Typeable as Export
#else
import Data.Proxy    as Export
import Data.Typeable as Export
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
{-# INLINE typeRep #-}
#endif
