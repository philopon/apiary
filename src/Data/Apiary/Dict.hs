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
    ( -- * dictionary
      Dict
    , KV(..)
    , empty

      -- * insert
    , type (</)
    , add

      -- * get
    , Member
    , get

      -- * convenient
    , Members
    , key
    ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote(QuasiQuoter(..))

import GHC.Exts(Constraint)

import Data.Apiary.Compat
import Data.Apiary.Tree

#if __GLASGOW_HASKELL__ > 707
import GHC.TypeLits
#endif

import Unsafe.Coerce

data KV v = Symbol := v

newtype Dict (kvs :: [KV *]) = Dict Tree

empty :: Dict '[]
empty = Dict Tip

-- | result type for pretty printing type error.
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

-- | 'not elem key' constraint(ghc >= 7.8)
type k </ v = HasKey k v ~ AlreadyExists k

-- | add key value pair to dictionary.
add :: (k </ kvs) => proxy k -> v -> Dict kvs -> Dict (k := v ': kvs)
add _ v (Dict d) = Dict (unsafeCoerce v `cons` d)

#if __GLASGOW_HASKELL__ > 707
type family Ix (k :: Symbol) (kvs :: [KV *]) :: Nat where
  Ix k (k  := v ': kvs) = 0
  Ix k (k' := v ': kvs) = 1 + Ix k kvs

getImpl :: forall proxy k kvs v. KnownNat (Ix k kvs) => proxy (k :: Symbol) -> Dict kvs -> v
getImpl _ (Dict d) = unsafeCoerce $ d `index` fromIntegral (natVal (Proxy :: Proxy (Ix k kvs)))

class Member (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: proxy k -> Dict kvs -> v

instance Member k v (k := v ': kvs) where
    get' = getImpl

instance (Member k v kvs, KnownNat (Ix k (k' := v' ': kvs))) => Member k v (k' := v' ': kvs) where
    get' = getImpl

get = get'
#else
class Member (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: Int -> proxy k -> Dict kvs -> v

instance Member k v (k := v ': kvs) where
    get' !i _ (Dict d) = unsafeCoerce $ d V.! i

instance Member k v kvs => Member k v (k' := v' ': kvs) where
    get' !i k d = get' (i + 1) k (unsafeCoerce d :: Dict kvs)

get = get' 0
#endif

-- | get key from dictionary
get :: Member k v kvs => proxy k -> Dict kvs -> v

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
