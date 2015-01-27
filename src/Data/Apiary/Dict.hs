{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Data.Apiary.Dict
    ( empty, add
    , KV(..), HasKeyResult(..)
    , HasKey, type (</), type (<:)
    , Dict
    , Member, Members, get
    , key
#if __GLASGOW_HASKELL__ > 707
    , Ix
#endif
    ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote(QuasiQuoter(..))

import GHC.Exts(Any, Constraint)

import qualified Data.Vector as V

import Data.Apiary.Compat

#if __GLASGOW_HASKELL__ > 707
import GHC.TypeLits
#endif

import Unsafe.Coerce

empty :: Dict '[]
empty = Dict V.empty

data KV v = Symbol := v

newtype Dict (kvs :: [KV *]) = Dict (V.Vector Any)

data HasKeyResult
    = AlreadyExists Symbol
    | Dictionary

#if __GLASGOW_HASKELL__ > 707
type family HasKey (k :: Symbol) (kvs :: [KV *]) :: HasKeyResult where
  HasKey k '[] = AlreadyExists k
  HasKey k (k  := v ': kvs) = Dictionary
  HasKey k (k' := v ': kvs) = HasKey k kvs
#else
type family HasKey (k :: Symbol) (kvs :: [KV *]) :: HasKeyResult
type instance HasKey k kvs = AlreadyExists k
#endif

type k </ v = HasKey k v ~ AlreadyExists k
type k <: v = HasKey k v ~ Dictionary

add :: (k </ kvs) => proxy k -> v -> Dict kvs -> Dict (k := v ': kvs)
add _ v (Dict d) = Dict (unsafeCoerce v `V.cons` d)

#if __GLASGOW_HASKELL__ > 707
type family Ix (k :: Symbol) (kvs :: [KV *]) :: Nat where
  Ix k (k  := v ': kvs) = 0
  Ix k (k' := v ': kvs) = 1 + Ix k kvs

getImpl :: forall proxy k kvs v. KnownNat (Ix k kvs) => proxy (k :: Symbol) -> Dict kvs -> v
getImpl _ (Dict d) = unsafeCoerce $ d V.! fromIntegral (natVal (Proxy :: Proxy (Ix k kvs)))

class Member (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get :: proxy k -> Dict kvs -> v

instance Member k v (k := v ': kvs) where
    get = getImpl

instance (Member k v kvs, KnownNat (Ix k (k' := v' ': kvs))) => Member k v (k' := v' ': kvs) where
    get = getImpl
#else
class Member (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: Int -> proxy k -> Dict kvs -> v

instance Member k v (k := v ': kvs) where
    get' !i _ (Dict d) = unsafeCoerce $ d V.! i

instance Member k v kvs => Member k v (k' := v' ': kvs) where
    get' !i k d = get' (i + 1) k (unsafeCoerce d :: Dict kvs)

get :: Member k v kvs => proxy k -> Dict kvs -> v
get = get' 0
#endif

-- | type family to constraint multi kvs.
--
-- > Members ["foo" := Int, "bar" := Double] prms == (Member "foo" Int prms, Member "bar" Double prms)
--
type family Members (kvs :: [KV *]) (prms :: [KV *]) :: Constraint
type instance Members '[] prms = ()
type instance Members (k := v ': kvs) prms = (Member k v prms, Members kvs prms)

-- | construct string literal proxy.
--
-- prop> [key|foo|] == (Proxy :: Proxy "foo")
--
key :: QuasiQuoter
key = QuasiQuoter
    { quoteExp  = \s -> [| SProxy :: SProxy $(TH.litT $ TH.strTyLit s) |]
    , quotePat  = error "key qq only exp or type."
    , quoteType = \s -> [t| SProxy $(TH.litT $ TH.strTyLit s) |]
    , quoteDec  = error "key qq only exp or type."
    }
