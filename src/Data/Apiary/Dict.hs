{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Apiary.Dict where

import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.TypeLits(Symbol)
import GHC.Exts

data Elem = forall a. Symbol := a

data Dict (ks :: [Elem]) where
    Empty :: Dict '[]
    Insert :: proxy (k :: Symbol) -> v -> Dict ks -> Dict (k := v ': ks)

class Member (k :: Symbol) (v :: *) (ks :: [Elem]) | k ks -> v where
    get :: proxy k -> Dict ks -> v

instance Member k v (k := v ': ks) where
    get _ (Insert _ v _) = v

instance Member k v ks => Member k v (k' := v' ': ks) where
    get p (Insert _ _ d) = get p d

#if __GLASGOW_HASKELL__ >= 708
type family Member' (k::Symbol) (vs :: [Elem]) :: Bool where
    Member' k  '[] = False
    Member' k  (k := v ': kvs) = True
    Member' k' (k := v ': kvs) = Member' k' kvs
#else
type family   Member' (k::Symbol) (vs :: [Elem]) :: Bool
type instance Member' k kvs = False
#endif

type family Members (kvs :: [Elem]) (prms :: [Elem]) :: Constraint
type instance Members '[] prms = ()
type instance Members (k := v ': kvs) prms = (Member k v prms, Members kvs prms)

type NotMember k kvs = Member' k kvs ~ False

insert :: NotMember k ks => proxy k -> v -> Dict ks -> Dict (k := v ': ks)
insert = Insert

key :: QuasiQuoter
key = QuasiQuoter
    { quoteExp  = \s -> [| Proxy :: Proxy $(litT $ strTyLit s) |]
    , quotePat  = error "key qq only exp or type."
    , quoteType = \s -> [t| Proxy $(litT $ strTyLit s) |]
    , quoteDec  = error "key qq only exp or type."
    }
