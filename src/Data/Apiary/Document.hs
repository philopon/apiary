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
dtail _       just (DocPath   _     d) = just d
dtail _       just (DocRoot         d) = just d
dtail _       just (DocFetch  _     d) = just d
dtail _       just (DocMethod _     d) = just d
dtail _       just (DocQuery  _ _ _ d) = just d
dtail _       just (DocGroup  _     d) = just d
dtail nothing _    l                   = nothing l

dlast :: Doc -> Doc
dlast = dtail id dlast

data Doc
    = DocPath   T.Text    Doc
    | DocRoot             Doc
    | DocFetch  TypeRep   Doc
    | DocMethod HT.Method Doc
    | DocQuery  S.ByteString StrategyRep QueryRep Doc
    | DocGroup  T.Text    Doc
    | Document  (Maybe T.Text)
    deriving (Show, Eq)

data Route 
    = Path  T.Text  Route
    | Fetch TypeRep Route
    | End
    deriving (Show, Eq)

extractRoute :: Doc -> Route
extractRoute = loop id
  where
    loop f (DocPath  t d) = loop (f . Path  t) d
    loop f (DocFetch t d) = loop (f . Fetch t) d
    loop _ (DocRoot    _) = Path "/" End
    loop f d              = dtail (const $ f End) (loop f) d

type Documents = [(Maybe T.Text, PathDoc)]

data PathDoc = PathDoc
    { path    :: Route
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
docToDocument (DocGroup g d) = pathDoc [] (\p -> [(Just  g, p)]) d
docToDocument             d  = pathDoc [] (\p -> [(Nothing, p)]) d

getMethod :: Doc -> HT.Method
getMethod (DocMethod m _) = m
getMethod d               = dtail (const "Any") getMethod d

pathDoc :: a -> (PathDoc -> a) -> Doc -> a
pathDoc nothing just d = methodDoc nothing (\methDoc -> 
    just $ PathDoc (extractRoute d) [(getMethod d, methDoc)]
    ) d

getQueries :: Doc -> [(S.ByteString, StrategyRep, QueryRep)]
getQueries (DocQuery q s r d) = (q,s,r) :        getQueries d
getQueries d                  = dtail (const []) getQueries d

methodDoc :: a -> (MethodDoc -> a) -> Doc -> a
methodDoc nothing just d = case dlast d of
    Document (Just t) -> just $ MethodDoc (getQueries d) t
    _                 -> nothing
