{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Apiary.SList where

import GHC.Exts(Constraint)

data SList (as :: [*]) where
    SNil  :: SList '[]
    (:::) :: a -> SList xs -> SList (a ': xs)

infixr :::

type family All (c :: * -> Constraint) (as :: [*]) :: Constraint
type instance All c '[] = ()
type instance All c (a ': as) = (c a, All c as)

deriving instance All Show as => Show (SList as)

type family Apply (as :: [*]) r
type instance Apply '[] r = r
type instance Apply (x ': xs) r = x -> Apply xs r

type Fn c a = Apply (Reverse c) a
apply :: Fn c r -> SList c -> r
apply f l = apply' f $ sReverse l

apply' :: Apply xs r -> SList xs -> r
apply' v SNil = v
apply' f (a ::: as) = apply' (f a) as

type family Rev (l :: [*]) (a :: [*]) :: [*]
type instance Rev '[] a = a
type instance Rev (x ': xs) a = Rev xs (x ': a)

type Reverse (a :: [*]) = Rev a '[]

sReverse :: SList as -> SList (Reverse as)
sReverse l = rev l SNil
  where
    rev :: SList as -> SList bs -> SList (Rev as bs)
    rev SNil a = a
    rev (x:::xs) a = rev xs (x:::a)

