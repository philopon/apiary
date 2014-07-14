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

description :: Monad m => String -> m (String, Maybe String)
description s = case break (== '(') s of
    (t, []) -> return (t, Nothing)
    (t, st) -> case break (== ')') st of
        (_:b, ")") -> return (t, Just b)
        (_, _)     -> fail "capture: syntax error." 

mkCap :: [String] -> ExpQ
mkCap [] = [|Capture.endPath|]
mkCap ((':':tyStr):as) = do
    (t, mbd) <- description tyStr
    ty <- lookupTypeName t >>= maybe (fail $ t ++ " not found.") return
    let d = case mbd of
            Nothing -> [|Nothing|]
            Just h  -> [|Just $(stringE h)|]
    [|Capture.fetch (Proxy :: Proxy $(conT ty)) $d . $(mkCap as)|]
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

