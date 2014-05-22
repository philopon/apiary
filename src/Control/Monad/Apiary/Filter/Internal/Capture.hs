{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Control.Monad.Apiary.Filter.Internal.Capture where

import Network.Wai

import Control.Applicative
import qualified Data.Text as T
import Data.Apiary.Param
import Data.Apiary.SList
import Data.Proxy

import Control.Monad.Apiary
import Control.Monad.Apiary.Filter.Internal

data Equal   = Equal T.Text
type Fetch = Proxy

class CaptureElem a where
  type Next a (xs :: [*]) :: [*]
  captureElem :: a -> T.Text -> SList xs -> Maybe (SList (Next a xs))

instance CaptureElem Equal where
    type Next Equal xs = xs
    captureElem (Equal s) p c | s == p    = Just c
                              | otherwise = Nothing

instance Path a => CaptureElem (Fetch a) where
    type Next (Fetch a) xs = (xs `Snoc` a)
    captureElem (Proxy :: Fetch a) p c = (sSnoc c) <$> (readPath p :: Maybe a)

type Capture as = All CaptureElem as

type family   CaptureResult (bf :: [*]) (as :: [*]) :: [*]
type instance CaptureResult bf '[] = bf
type instance CaptureResult bf (a ': as) = (CaptureResult (Next a bf) as)

capture' :: Capture as => SList as -> [T.Text] -> SList xs -> Maybe (SList (CaptureResult xs as))
capture' SNil       []     bf = Just bf
capture' (c ::: cs) (p:ps) bf = captureElem c p bf >>= capture' cs ps
capture' SNil _ _ = Nothing
capture' _ [] _   = Nothing

-- | low level (without Template Haskell) capture. since 0.4.2.0
--
-- @
-- myCapture :: 'SList' '['Equal', 'Fetch' Int, Fetch String]
-- myCapture = 'Equal' \"path\" ':::' 'pInt' ::: 'pString' ::: 'SNil'
--
-- capture myCapture . stdMethod GET . action $ \age name -> do
--     yourAction
-- @
capture :: (Functor n, Monad n) => Capture as => SList as 
        -> ApiaryT' (CaptureResult xs as) n m b -> ApiaryT' xs n m b
capture cap = function $ \bf req -> capture' cap (pathInfo req) bf
