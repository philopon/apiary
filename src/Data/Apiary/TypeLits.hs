{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Apiary.TypeLits 
    ( Symbol, KnownSymbol, symbolVal
    ) where

import GHC.TypeLits

#if __GLASGOW_HASKELL__ < 708
type KnownSymbol n = SingRep n String

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (sing :: Sing n)
#endif
