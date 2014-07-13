{-# LANGUAGE OverloadedStrings #-}

module Data.Apiary.Document where

import qualified Data.Text as T
import qualified Data.ByteString as S
import Data.Typeable
import Data.Apiary.Param
import qualified Network.HTTP.Types as HT


data StrategyRep = StrategyRep
    { strategyInfo :: T.Text }
    deriving (Show, Eq)

dtail :: (Doc -> a) -> (Doc -> a) -> Doc -> a
dtail _       just (Path   _     d) = just d
dtail _       just (Root         d) = just d
dtail _       just (Fetch  _     d) = just d
dtail _       just (Method _     d) = just d
dtail _       just (Query  _ _ _ d) = just d
dtail _       just (Group  _     d) = just d
dtail nothing _    l                = nothing l

dlast :: Doc -> Doc
dlast = dtail id dlast

data Doc
    = Path   T.Text    Doc
    | Root             Doc
    | Fetch  TypeRep   Doc
    | Method HT.Method Doc
    | Query  S.ByteString StrategyRep QueryRep Doc
    | Group  T.Text    Doc
    | Leaf   (Maybe T.Text)
    deriving (Show, Eq)

extractPath :: Doc -> Doc
extractPath (Path  t     d) = Path  t (extractPath d)
extractPath (Fetch t     d) = Fetch t (extractPath d)
extractPath (Root  _)       = Root $ Leaf Nothing
extractPath d               = dtail (const $ Leaf Nothing) extractPath d

type Documents = [(Maybe T.Text, PathDoc)]

data PathDoc = PathDoc
    { path    :: Doc
    , methods :: [(HT.Method, MethodDoc)]
    } deriving Show

data MethodDoc = MethodDoc
    { queries  :: [(S.ByteString, StrategyRep, QueryRep)]
    , document :: T.Text
    } deriving Show

docsToDocuments :: [Doc] -> Documents
docsToDocuments d = merge $ concatMap docToDocument d
  where
    same (g, pd) (g', pd') = g == g' && path pd == path pd'
    merge []            = []
    merge (f@(g, pd):o) = 
        let sameDocs = concatMap (methods . snd) $ filter (same f) o
            newDoc   = (g, PathDoc (path pd) $ methods pd ++ sameDocs)
        in newDoc : merge (filter (not . same f) o)


docToDocument :: Doc -> Documents
docToDocument (Group g d) = pathDoc [] (\p -> [(Just  g, p)]) d
docToDocument          d  = pathDoc [] (\p -> [(Nothing, p)]) d

getMethod :: Doc -> HT.Method
getMethod (Method m _) = m
getMethod d            = dtail (const "Any") getMethod d

pathDoc :: a -> (PathDoc -> a) -> Doc -> a
pathDoc nothing just d = methodDoc nothing (\methDoc -> 
    just $ PathDoc (extractPath d) [(getMethod d, methDoc)]
    ) d

getQueries :: Doc -> [(S.ByteString, StrategyRep, QueryRep)]
getQueries (Query q s r d) = (q,s,r) :        getQueries d
getQueries d               = dtail (const []) getQueries d

methodDoc :: a -> (MethodDoc -> a) -> Doc -> a
methodDoc nothing just d = case dlast d of
    Leaf (Just t) -> just $ MethodDoc (getQueries d) t
    _             -> nothing
