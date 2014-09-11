{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Apiary.Filter.Internal.Capture.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Control.Monad.Apiary.Filter.Internal.Capture as Capture

import qualified Data.Text as T
import Data.String
import Data.Proxy

preCap :: String -> [String]
preCap ""  = []
preCap "/" = []
preCap ('/':p) = splitPath p
preCap p       = splitPath p

splitPath :: String -> [String]
splitPath = map T.unpack . T.splitOn "/" . T.pack

data Lookup = N Name | S String | None

description :: String -> Q (String, Lookup)
description s = case break (`elem` "([") s of
    (t, []) -> return (t, None)
    (t, st) -> case break (`elem` ")]") st of
        (_:'$':b, ")") -> do
            reportWarning "DEPRECATED () description. use []."
            v <- lookupValueName b
            maybe (fail $ b ++ " not found.") (return . (t,) . N) v
        (_:b,     ")") -> do
            reportWarning "DEPRECATED () description. use []."
            return (t, S b)
        (_:'$':b, "]") -> lookupValueName b >>=
            maybe (fail $ b ++ " not found.") (return . (t,) . N)
        (_:b,     "]") -> return (t, S b)
        (_, _)         -> fail "capture: syntax error." 

mkCap :: [String] -> ExpQ
mkCap [] = [|Capture.endPath|]
mkCap ((':':tyStr):as) = do
    (t, mbd) <- description tyStr
    ty <- lookupTypeName t >>= maybe (fail $ t ++ " not found.") return
    let d = case mbd of
            None -> [|Nothing|]
            S h  -> [|Just $(stringE h)|]
            N n  -> [|Just $(varE n)|]
    [|Capture.fetch (Proxy :: Proxy $(conT ty)) $d . $(mkCap as)|]
mkCap ("**":as) = [|Capture.restPath . $(mkCap as) |]
mkCap (eq:as)   = [|(Capture.path (fromString $(stringE eq))) . $(mkCap as) |]

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

