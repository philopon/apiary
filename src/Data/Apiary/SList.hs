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

deriving instance All Show as => Show (SList as)

type family Fn (as :: [*]) r
type instance Fn '[] r = r
type instance Fn (x ': xs) r = x -> Fn xs r

type family Snoc (as :: [*]) a :: [*]
type instance Snoc '[] a = a ': '[]
type instance Snoc (x ': xs) a = x ': Snoc xs a

type family All (c :: * -> Constraint) (as :: [*]) :: Constraint
type instance All c '[] = ()
type instance All c (a ': as) = (c a, All c as)

apply :: Fn xs r -> SList xs -> r
apply v SNil = v
apply f (a ::: as) = apply (f a) as

sSnoc :: SList as -> a -> SList (Snoc as a)
sSnoc SNil       a = a ::: SNil
sSnoc (x ::: xs) a = x ::: sSnoc xs a

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

