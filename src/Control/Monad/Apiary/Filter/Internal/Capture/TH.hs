{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Apiary.Filter.Internal.Capture.TH where

import Control.Arrow
import Control.Applicative

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Control.Monad.Apiary.Filter.Internal.Capture as Capture

import qualified Data.Text as T
import Data.String
import Data.List
import Data.Proxy

preCap :: String -> [String]
preCap ""  = []
preCap "/" = []
preCap ('/':p) = splitPath p
preCap p       = splitPath p

splitPath :: String -> [String]
splitPath = map T.unpack . T.splitOn "/" . T.pack

--            S h  -> [|Just $(stringE h)|]
--            N n  -> [|Just $(varE n)|]
description :: String -> Q (String, ExpQ)
description s = case break (`elem` "([") s of
    (t, []) -> return (t, [|Nothing|])
    (t, st) -> case break (`elem` ")]") st of
        (_:'$':b, ")") -> do
            reportWarning "DEPRECATED () description. use []."
            v <- lookupValueName b
            maybe (fail $ b ++ " not found.") (\n -> return (t, [|Just $(varE n)|])) v
        (_:b,     ")") -> do
            reportWarning "DEPRECATED () description. use []."
            return (t, [|Just $(stringE b)|])
        (_:'$':b, "]") -> lookupValueName b >>=
            maybe (fail $ b ++ " not found.") (\n -> return (t, [|Just $(varE n)|]))
        (_:b,     "]") -> return (t, [|Just $(stringE b)|])
        (_, _)         -> fail "capture: syntax error." 

mkCap :: [String] -> ExpQ
mkCap [] = [|Capture.endPath|]
mkCap (('*':'*':[]):as) = [|Capture.anyPath . $(mkCap as) |]
mkCap (('*':'*':tS):as) = do
    (k, d) <- description tS
    [|Capture.restPath (Proxy :: Proxy $(litT $ strTyLit k)) $d . $(mkCap as) |]
mkCap (str:as)
    | "::" `isInfixOf` fst (break (`elem` "([") str) = do
        (key, d) <- first T.pack <$> description str
        let v = T.unpack . T.strip . fst $ T.breakOn    "::" key
            t = T.unpack . T.strip . snd $ T.breakOnEnd "::" key
        ty <- lookupTypeName t >>= maybe (fail $ t ++ " not found.") return
        [|(Capture.fetch (Proxy :: Proxy $(litT $ strTyLit v)) (Proxy :: Proxy $(conT ty)) $d) . $(mkCap as)|]

    | otherwise = [|(Capture.path (fromString $(stringE str))) . $(mkCap as) |]

-- | capture QuasiQuoter. since 0.1.0.0.
--
-- example:
--
-- @
-- [capture|\/path|] -- first path == "path"
-- [capture|\/int\/foo::Int|] -- first path == "int" && get 2nd path as Int.
-- [capture|\/bar::Int\/baz::Double|] -- get first path as Int and get 2nd path as Double.
-- [capture|/**baz|] -- feed greedy and get all path as [Text] (since 0.17.0). 
-- @
--
capture :: QuasiQuoter
capture = QuasiQuoter 
    { quoteExp = mkCap . preCap
    , quotePat  = \_ -> error "No quotePat."
    , quoteType = \_ -> error "No quoteType."
    , quoteDec  = \_ -> error "No quoteDec."
    }

