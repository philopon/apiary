{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Apiary.Filter.Internal.Capture.TH(capture) where

import Control.Arrow(first)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote(QuasiQuoter(..))

import qualified Control.Monad.Apiary.Filter.Capture as Capture

import qualified Data.Text as T
import Data.String(IsString(..))
import Data.List(isInfixOf)
import Data.Apiary.SProxy(SProxy(..))
import Data.Proxy.Compat(Proxy(..))

preCap :: String -> [String]
preCap ""  = []
preCap "/" = []
preCap ('/':p) = splitPath p
preCap p       = splitPath p

splitPath :: String -> [String]
splitPath = map T.unpack . T.splitOn "/" . T.pack

description :: String -> TH.Q (String, TH.ExpQ)
description s = case break (`elem` ("([" :: String)) s of
    (t, []) -> return (t, [|Nothing|])
    (t, st) -> case break (`elem` (")]" :: String)) st of
        (_:'$':b, ")") -> do
            TH.reportWarning "DEPRECATED () description. use []."
            v <- TH.lookupValueName b
            maybe (fail $ b ++ " not found.") (\n -> return (t, [|Just $(TH.varE n)|])) v
        (_:b,     ")") -> do
            TH.reportWarning "DEPRECATED () description. use []."
            return (t, [|Just $(TH.stringE b)|])
        (_:'$':b, "]") -> TH.lookupValueName b >>=
            maybe (fail $ b ++ " not found.") (\n -> return (t, [|Just $(TH.varE n)|]))
        (_:b,     "]") -> return (t, [|Just $(TH.stringE b)|])
        (_, _)         -> fail "capture: syntax error." 

mkCap :: [String] -> TH.ExpQ
mkCap [] = [|id|]
mkCap (('*':'*':[]):as) = [|Capture.anyPath . $(mkCap as) |]
mkCap (('*':'*':tS):as) = do
    (k, d) <- description tS
    [|Capture.restPath (SProxy :: SProxy $(TH.litT $ TH.strTyLit k)) $d . $(mkCap as) |]
mkCap (str:as)
    | "::" `isInfixOf` fst (break (`elem` ("([" :: String)) str) = do
        (key, d) <- first T.pack `fmap` description str
        let v = T.unpack . T.strip . fst $ T.breakOn    "::" key
            t = T.unpack . T.strip . snd $ T.breakOnEnd "::" key
        ty <- TH.lookupTypeName t >>= maybe (fail $ t ++ " not found.") return
        [|(Capture.fetch' (SProxy :: SProxy $(TH.litT $ TH.strTyLit v)) (Proxy :: Proxy $(TH.conT ty)) $d) . $(mkCap as)|]

    | otherwise = [|(Capture.path (fromString $(TH.stringE str))) . $(mkCap as) |]

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
-- this QQ can convert pure function easily.
--
-- @
-- [capture|/foo/foo::Int|]        == path "path" . fetch (Proxy :: Proxy ("foo" := Int)) . endPath
-- [capture|/bar/bar::Int/**rest|] == path "path" . fetch (Proxy :: Proxy ("foo" := Int)) . restPath (Proxy :: Proxy "rest")
-- @
--
capture :: QuasiQuoter
capture = QuasiQuoter 
    { quoteExp = mkCap . preCap
    , quotePat  = \_ -> error "No quotePat."
    , quoteType = \_ -> error "No quoteType."
    , quoteDec  = \_ -> error "No quoteDec."
    }

