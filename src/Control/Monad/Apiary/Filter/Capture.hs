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

module Control.Monad.Apiary.Filter.Capture where

import Network.Wai

import Control.Applicative
import qualified Data.Text as T
import Data.Apiary.Param
import Data.Apiary.SList

import Control.Monad.Apiary

data Equal   = Equal T.Text
data Fetch a = Fetch

class CaptureElem a where
  type Next a (xs :: [*]) :: [*]
  captureElem :: a -> T.Text -> SList xs -> Maybe (SList (Next a xs))

instance CaptureElem Equal where
    type Next Equal xs = xs
    captureElem (Equal s) p c | s == p    = Just c
                              | otherwise = Nothing

instance Param a => CaptureElem (Fetch a) where
    type Next (Fetch a) xs = (xs `Snoc` a)
    captureElem (Fetch :: Fetch a) p c = (sSnoc c) <$> (readPath p :: Maybe a)


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
-- myCapture :: SList '[Equal, Fetch Int, Fetch String]
-- myCapture = Equal "path" ::: (Fetch :: Fetch Int) ::: (Fetch :: Fetch String) ::: SNil
--
-- capture myCapture . stdMethod GET . action $ \age name -> do
--     yourAction
-- @
capture :: (Capture as, Monad m) => SList as -> ApiaryT (CaptureResult xs as) m b -> ApiaryT xs m b
capture cap = function $ \bf req -> capture' cap (pathInfo req) bf
