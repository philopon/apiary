{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Apiary.Filter.Internal.Capture.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Control.Monad.Apiary.Filter.Internal.Capture as Capture
import qualified Data.Text as T
import Data.Proxy

preCap :: String -> [String]
preCap ""  = []
preCap "/" = []
preCap ('/':p) = splitPath p
preCap p       = splitPath p

splitPath :: String -> [String]
splitPath = map T.unpack . T.splitOn "/" . T.pack

mkCap :: [String] -> ExpQ
mkCap [] = [|Capture.endPath|]
mkCap ((':':tyStr):as) = do
    -- ty <- lookupTypeName tyStr >>= maybe (fail "") return
    let ty = mkName tyStr
    [|Capture.fetch (Proxy :: Proxy $(conT ty)) . $(mkCap as) |]
mkCap (eq:as) = do
    [|(Capture.path $(stringE eq)) . $(mkCap as) |]

-- | capture QuasiQuoter. since 0.1.0.0.
--
-- example:
--
-- @
-- [capture|\/path|] -- first path == "path"
-- [capture|\/int\/:Int|] -- first path == "int" && get 2nd path as Int.
-- [capture|\/:Int\/:Double|] -- get first path as Int and get 2nd path as Double.
-- @
--
capture :: QuasiQuoter
capture = QuasiQuoter 
    { quoteExp = mkCap . preCap
    , quotePat  = \_ -> error "No quotePat."
    , quoteType = \_ -> error "No quoteType."
    , quoteDec  = \_ -> error "No quoteDec."
    }

