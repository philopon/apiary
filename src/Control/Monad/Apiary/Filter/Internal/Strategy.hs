{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Apiary.Filter.Internal.Strategy where

import Data.Maybe
import Data.Reflection
import qualified Data.Text as T

import Data.Apiary.Proxy
import Data.Apiary.SList
import Data.Apiary.Document

class Strategy (w :: * -> *) where
    type SNext w (as :: [*]) a  :: [*]
    readStrategy :: (v -> Maybe a)
                 -> ((k,v) -> Bool)
                 -> proxy (w a)
                 -> [(k, v)]
                 -> SList as 
                 -> Maybe (SList (SNext w as a))
    strategyRep :: proxy w -> StrategyRep

getQuery :: (v -> Maybe a) -> proxy (w a) -> ((k,v) -> Bool) -> [(k, v)] -> [Maybe a]
getQuery readf _ kf = map readf . map snd . filter kf

-- | get first matched key( [0,) params to Type.). since 0.5.0.0.
data Option a deriving Typeable
instance Strategy Option where
    type SNext Option as a = Maybe a ': as
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else Just . (::: l) $ case catMaybes rs of
               []  -> Nothing
               a:_ -> Just a
    strategyRep _ = StrategyRep "optional"

-- | get first matched key ( [1,) params to Maybe Type.) since 0.5.0.0.
data First a deriving Typeable
instance Strategy First where
    type SNext First as a = a ': as
    readStrategy rf k p q l =
        case getQuery rf p k q of
            Just a:_ -> Just $ a ::: l
            _        -> Nothing
    strategyRep _ = StrategyRep "first"

-- | get key ( [1,1] param to Type.) since 0.5.0.0.
data One a deriving Typeable
instance Strategy One where
    type SNext One as a = a ': as
    readStrategy rf k p q l =
        case getQuery rf p k q of
            [Just a] -> Just $ a ::: l
            _        -> Nothing
    strategyRep _ = StrategyRep "one"

-- | get parameters ( [0,) params to [Type] ) since 0.5.0.0.
data Many a deriving Typeable
instance Strategy Many where
    type SNext Many as a = [a] ': as
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else Just $ (catMaybes rs) ::: l
    strategyRep _ = StrategyRep "many"

-- | get parameters ( [1,) params to [Type] ) since 0.5.0.0.
data Some a deriving Typeable
instance Strategy Some where
    type SNext Some as a = [a] ': as
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               as -> Just $ as ::: l
    strategyRep _ = StrategyRep "some"

-- | get parameters with upper limit ( [1,n] to [Type]) since 0.6.0.0.
data LimitSome u a deriving Typeable
instance (Reifies u Int) => Strategy (LimitSome u) where
    type SNext (LimitSome u) as a = [a] ': as
    readStrategy rf k p q l =
        let rs = take (reflectLimit p) $ getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               as -> Just $ as ::: l
    strategyRep _ = StrategyRep . T.pack $ "less then " ++ show (reflect (Proxy :: Proxy u))

reflectLimit :: Reifies n Int => proxy (LimitSome n a) -> Int
reflectLimit p = reflect $ asTyInt p
  where
    asTyInt :: proxy (LimitSome u a) -> Proxy u
    asTyInt _ = Proxy

-- | type check ( [0,) params to No argument ) since 0.5.0.0.
data Check a deriving Typeable
instance Strategy Check where
    type SNext Check as a = as
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case  catMaybes rs of
               [] -> Nothing
               _  -> Just l
    strategyRep _ = StrategyRep "check"

-- | construct Option proxy. since 0.5.1.0.
pOption :: proxy a -> Proxy (Option a)
pOption _ = Proxy

-- | construct First proxy. since 0.5.1.0.
pFirst :: proxy a -> Proxy (First a)
pFirst _ = Proxy

-- | construct One proxy. since 0.5.1.0.
pOne :: proxy a -> Proxy (One a)
pOne _ = Proxy

-- | construct Many proxy. since 0.5.1.0.
pMany :: proxy a -> Proxy (Many a)
pMany _ = Proxy

-- | construct Some proxy. since 0.5.1.0.
pSome :: proxy a -> Proxy (Some a)
pSome _ = Proxy

-- | construct Check proxy. since 0.5.1.0.
pCheck :: proxy a -> Proxy (Check a)
pCheck _ = Proxy
