{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Apiary.TH.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Monad.Apiary.Filter.Capture hiding(capture, capture')
import qualified Control.Monad.Apiary.Filter.Capture as Capture
import Data.Apiary.SList
import qualified Data.Text as T

preCap :: String -> [String]
preCap ""  = []
preCap "/" = []
preCap ('/':p) = splitPath p
preCap p       = splitPath p

splitPath :: String -> [String]
splitPath = map T.unpack . T.splitOn "/" . T.pack

mkCap :: [String] -> ExpQ
mkCap [] = [|SNil|]
mkCap ((':':tyStr):as) = do
    -- ty <- lookupTypeName tyStr >>= maybe (fail "") return
    let ty = mkName tyStr
    [|(Fetch :: Fetch $(conT ty)) ::: $(mkCap as) |]
mkCap (eq:as) = do
    [|(Equal $(stringE eq)) ::: $(mkCap as) |]

applyCapture :: ExpQ -> ExpQ
applyCapture e = [|Capture.capture $e|]

capture :: QuasiQuoter
capture = QuasiQuoter 
    { quoteExp = applyCapture . mkCap . preCap
    , quotePat  = \_ -> error "No quotePat."
    , quoteType = \_ -> error "No quoteType."
    , quoteDec  = \_ -> error "No quoteDec."
    }
