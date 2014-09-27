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

module Data.Apiary.Dict where

import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.TypeLits(Symbol)

data Dict (ks :: [(Symbol, *)]) where
    Empty :: Dict '[]
    Insert :: proxy (k :: Symbol) -> v -> Dict ks -> Dict ('(k,v) ': ks)

class Member (k :: Symbol) (ks :: [(Symbol, *)]) v | k ks -> v where
    get :: proxy k -> Dict ks -> v

instance Member k ('(k, v) ': ks) v where
    get _ (Insert _ v _) = v

instance Member k ks v => Member k ('(k', v') ': ks) v where
    get p (Insert _ _ d) = get p d

#if __GLASGOW_HASKELL__ >= 708
type family Member' (k::Symbol) (vs :: [(Symbol, *)]) :: Bool where
    Member' k  '[] = False
    Member' k  ('(k,v) ': kvs) = True
    Member' k' ('(k,v) ': kvs) = Member' k' kvs
#else
type family   Member' (k::Symbol) (vs :: [(Symbol, *)]) :: Bool
type instance Member' k kvs = False
#endif

type NotMember k kvs = Member' k kvs ~ False

insert :: NotMember k ks => proxy k -> v -> Dict ks -> Dict ('(k, v) ': ks)
insert = Insert

key :: QuasiQuoter
key = QuasiQuoter
    { quoteExp  = \s -> [| Proxy :: Proxy $(litT $ strTyLit s) |]
    , quotePat  = error "key qq only exp or type."
    , quoteType = \s -> [t| Proxy $(litT $ strTyLit s) |]
    , quoteDec  = error "key qq only exp or type."
    }
