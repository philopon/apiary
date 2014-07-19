{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module Data.Apiary.Proxy
    ( module Export
    , typeRep
    ) where

#if __GLASGOW_HASKELL__ > 707
import qualified Data.Typeable
import Data.Typeable as Export hiding (typeRep)
typeRep   = Data.Typeable.typeRep
#else
import Data.Proxy as Export
import Data.Typeable as Export
typeRep _ = typeOf (undefined :: a)
#endif
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
{-# INLINE typeRep #-}
