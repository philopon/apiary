{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.Apiary.Filter.Internal.Strategy where

import Data.Apiary.SList

import Data.Maybe
import Data.Proxy
import Data.Reflection

class Strategy (w :: * -> *) where
  type SNext w (as :: [*]) a  :: [*]
  readStrategy :: (v -> Maybe a)
               -> (k -> Bool)
               -> Proxy (w a)
               -> [(k, v)]
               -> SList as 
               -> Maybe (SList (SNext w as a))

getQuery :: (v -> Maybe a) -> Proxy (w a) -> (k -> Bool) -> [(k, v)] -> [Maybe a]
getQuery readf _ kf = map readf . map snd . filter (kf . fst)


-- | get first matched key( [1,) params to Type.). since 0.5.0.0.
data Option a
instance Strategy Option where
    type SNext Option as a = Snoc as (Maybe a)
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else Just . sSnoc l $ case catMaybes rs of
               []  -> Nothing
               a:_ -> Just a

-- | get first matched key ( [0,) params to Maybe Type.) since 0.5.0.0.
data First a
instance Strategy First where
    type SNext First as a = Snoc as a
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               a:_ -> Just $ sSnoc l a

-- | get key ( [1] param to Type.) since 0.5.0.0.
data One a
instance Strategy One where
    type SNext One as a = Snoc as a
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [a] -> Just $ sSnoc l a
               _   -> Nothing

-- | get parameters ( [0,) params to [Type] ) since 0.5.0.0.
data Many a
instance Strategy Many where
    type SNext Many as a = Snoc as [a]
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else Just $ sSnoc l (catMaybes rs)

-- | get parameters ( [1,) params to [Type] ) since 0.5.0.0.
data Some a
instance Strategy Some where
    type SNext Some as a = Snoc as [a]
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               as -> Just $ sSnoc l as

-- | get parameters with upper limit ( [1,n] to [Type]) since 0.5.2.0.
data LimitSome u a
instance (Reifies u Int) => Strategy (LimitSome u) where
    type SNext (LimitSome u) as a = Snoc as [a]
    readStrategy rf k p q l =
        let rs = take (reflectLimit p) $ getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               as -> Just $ sSnoc l as

reflectLimit :: Reifies n Int => Proxy (LimitSome n a) -> Int
reflectLimit p = reflect $ asTyInt p
  where
    asTyInt :: Proxy (LimitSome u a) -> Proxy u
    asTyInt _ = Proxy


-- | type check ( [0,) params to No argument ) since 0.5.0.0.
data Check a
instance Strategy Check where
    type SNext Check as a = as
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case  catMaybes rs of
               [] -> Nothing
               _  -> Just l

-- | construct Option proxy. since 0.5.1.0.
pOption :: Proxy a -> Proxy (Option a)
pOption _ = Proxy

-- | construct First proxy. since 0.5.1.0.
pFirst :: Proxy a -> Proxy (First a)
pFirst _ = Proxy

-- | construct One proxy. since 0.5.1.0.
pOne :: Proxy a -> Proxy (One a)
pOne _ = Proxy

-- | construct Many proxy. since 0.5.1.0.
pMany :: Proxy a -> Proxy (Many a)
pMany _ = Proxy

-- | construct Some proxy. since 0.5.1.0.
pSome :: Proxy a -> Proxy (Some a)
pSome _ = Proxy

-- | construct Check proxy. since 0.5.1.0.
pCheck :: Proxy a -> Proxy (Check a)
pCheck _ = Proxy
