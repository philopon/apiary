{-# LANGUAGE BangPatterns #-}

module Data.Apiary.Tree
    ( Tree(Tip)
    , cons
    , index
    , ) where

import GHC.Exts(Any)

data Dir = L | R

data Tree
    = Branch !Dir !Any !Tree !Tree
    | Tip

cons :: Any -> Tree -> Tree
cons v Tip = Branch L v Tip Tip
cons v (Branch _ a Tip Tip) = Branch R v (Branch L a Tip Tip) Tip
cons v (Branch _ a l   Tip) = Branch L v l (Branch L a Tip Tip)
cons v (Branch L a l   r)   = Branch R v (cons a l) r
cons v (Branch R a l   r)   = Branch L v l (cons a r)

index :: Tree -> Int -> Any
index Tip _ = error "out of range"
index (Branch _ v _ _) 0 = v
index (Branch L _ l r) !i | even i    = index l (i `quot` 2 - 1)
                          | otherwise = index r (i `quot` 2)
index (Branch R _ l r) !i | odd i     = index l (i `quot` 2)
                          | otherwise = index r (i `quot` 2 - 1)
