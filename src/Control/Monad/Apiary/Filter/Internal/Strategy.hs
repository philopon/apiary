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
import qualified Data.Text as T

import Data.Apiary.Proxy
import Data.Apiary.SList
import Data.Apiary.Document

class Strategy (w :: * -> *) where
    type SNext w (as :: [*]) a  :: [*]
    readStrategy :: (v -> Maybe a)
                 -> ((k,v) -> Bool)
                 -> w a
                 -> [(k, v)]
                 -> SList as 
                 -> Maybe (SList (SNext w as a))
    strategyRep :: forall a. w a -> StrategyRep

getQuery :: (v -> Maybe a) -> w a -> ((k,v) -> Bool) -> [(k, v)] -> [Maybe a]
getQuery readf _ kf = map readf . map snd . filter kf

-- | get first matched key( [0,) params to Type.). since 0.5.0.0.
data Option a = Option deriving Typeable
instance Strategy Option where
    type SNext Option as a = Maybe a ': as
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else Just . (::: l) $ case catMaybes rs of
               a:_ -> Just a
               []  -> Nothing
    strategyRep _ = StrategyRep "optional"

data Optional a = Optional T.Text a deriving Typeable
instance Strategy Optional where
    type SNext Optional as a = a ': as
    readStrategy rf k p@(Optional _ def) q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else Just . (::: l) $ case catMaybes rs of
               a:_ -> a
               []  -> def
    strategyRep (Optional tr _) = StrategyRep $
        "default:" `T.append` tr

-- | get first matched key ( [1,) params to Maybe Type.) since 0.5.0.0.
data First a = First deriving Typeable
instance Strategy First where
    type SNext First as a = a ': as
    readStrategy rf k p q l =
        case getQuery rf p k q of
            Just a:_ -> Just $ a ::: l
            _        -> Nothing
    strategyRep _ = StrategyRep "first"

-- | get key ( [1,1] param to Type.) since 0.5.0.0.
data One a = One deriving Typeable
instance Strategy One where
    type SNext One as a = a ': as
    readStrategy rf k p q l =
        case getQuery rf p k q of
            [Just a] -> Just $ a ::: l
            _        -> Nothing
    strategyRep _ = StrategyRep "one"

-- | get parameters ( [0,) params to [Type] ) since 0.5.0.0.
data Many a = Many deriving Typeable
instance Strategy Many where
    type SNext Many as a = [a] ': as
    readStrategy rf k p q l =
        let rs = getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else Just $ (catMaybes rs) ::: l
    strategyRep _ = StrategyRep "many"

-- | get parameters ( [1,) params to [Type] ) since 0.5.0.0.
data Some a = Some deriving Typeable
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
data LimitSome a = LimitSome {-# UNPACK #-} !Int deriving Typeable
instance Strategy LimitSome where
    type SNext LimitSome as a = [a] ': as
    readStrategy rf k p@(LimitSome lim) q l =
        let rs = take lim $ getQuery rf p k q
        in if any isNothing rs
           then Nothing
           else case catMaybes rs of
               [] -> Nothing
               as -> Just $ as ::: l
    strategyRep (LimitSome lim) = StrategyRep . T.pack $ "less then " ++ show lim

-- | type check ( [0,) params to No argument ) since 0.5.0.0.
data Check a = Check deriving Typeable
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
pOption :: proxy a -> Option a
pOption _ = Option

-- | construct Optional proxy. since 0.16.0.
pOptional :: Show a => a -> Optional a
pOptional def = Optional (T.pack $ show def) def

-- | construct First proxy. since 0.5.1.0.
pFirst :: proxy a -> First a
pFirst _ = First

-- | construct One proxy. since 0.5.1.0.
pOne :: proxy a -> One a
pOne _ = One

-- | construct Many proxy. since 0.5.1.0.
pMany :: proxy a -> Many a
pMany _ = Many

-- | construct Some proxy. since 0.5.1.0.
pSome :: proxy a -> Some a
pSome _ = Some

-- | construct LimitSome proxy. since 0.16.0.
pLimitSome :: Int -> proxy a -> LimitSome a
pLimitSome lim _ = LimitSome lim

-- | construct Check proxy. since 0.5.1.0.
pCheck :: proxy a -> Check a
pCheck _ = Check
