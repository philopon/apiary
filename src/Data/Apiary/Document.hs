{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Apiary.Document where

import Control.Applicative

import Data.Typeable
import Data.Maybe
import Data.List
import Data.Function

import Data.Apiary.Param
import Data.Apiary.Method

import Text.Blaze.Html
import qualified Data.Text as T

newtype StrategyRep = StrategyRep
    { strategyInfo :: T.Text }
    deriving (Show, Eq)

data Doc
    = DocPath   T.Text       Doc
    | DocRoot                Doc
    | DocFetch  T.Text TypeRep (Maybe Html) Doc
    | DocRest   T.Text  (Maybe Html) Doc
    | DocAny    Doc
    | DocDropNext            Doc

    | DocMethod Method       Doc
    | DocQuery  T.Text StrategyRep QueryRep (Maybe Html) Doc
    | DocPrecondition Html   Doc
    | DocGroup  T.Text       Doc
    | Document  T.Text       Doc
    | Action

--------------------------------------------------------------------------------

data Route
    = Path  T.Text                      Route
    | Fetch T.Text TypeRep (Maybe Html) Route
    | Rest  T.Text (Maybe Html)
    | Any
    | End

instance Eq Route where
    Path    a   d == Path  b   d'   = a == b && d == d'
    Fetch k a _ d == Fetch l b _ d' = k == l && a == b && d == d'
    Rest  k _     == Rest  l _      = k == l
    Any           == Any            = True
    End           == End            = True
    _             == _              = False

data Documents = Documents
    { noGroup :: [PathDoc]
    , groups  :: [(T.Text, [PathDoc])]
    }

data PathDoc = PathDoc
    { path    :: Route
    , methods :: [(Method, [MethodDoc])]
    }

data QueryDoc = QueryDoc
    { queryName     :: T.Text
    , queryStrategy :: StrategyRep
    , queryRep      :: QueryRep
    , queryDocument :: (Maybe Html)
    }

data MethodDoc = MethodDoc
    { queries       :: [QueryDoc]
    , preconditions :: [Html]
    , document      :: T.Text
    }

--------------------------------------------------------------------------------

docToDocument :: Doc -> Maybe (Maybe T.Text, PathDoc)
docToDocument = \case
    (DocGroup "" d') -> (Nothing,) <$> loop id (\md -> [("*", md)]) id id Nothing d'
    (DocGroup g  d') -> (Just  g,) <$> loop id (\md -> [("*", md)]) id id Nothing d'
    d'               -> (Nothing,) <$> loop id (\md -> [("*", md)]) id id Nothing d'
  where
    loop ph mh qs ps doc     (DocDropNext       d) = loop ph mh qs ps doc (dropNext d)
    loop ph mh qs pc doc     (DocPath         t d) = loop (ph . Path t) mh qs pc doc d
    loop _  mh qs pc doc     (DocRoot           d) = loop (const $ Path "" End) mh qs pc doc d
    loop ph mh qs pc doc     (DocFetch    k t h d) = loop (ph . Fetch k t h) mh qs pc doc d
    loop ph mh qs pc doc     (DocRest     k   h d) = loop (ph . const (Rest k h)) mh qs pc doc d
    loop ph mh qs pc doc     (DocAny            d) = loop (ph . const Any) mh qs pc doc d
    loop ph _  qs pc doc     (DocMethod       m d) = loop ph (\md -> [(m, md)]) qs pc doc d
    loop ph mh qs pc doc     (DocQuery  p s q t d) = loop ph mh (qs . (QueryDoc p s q t:)) pc doc d
    loop ph mh qs pc doc     (DocPrecondition h d) = loop ph mh qs (pc . (h:)) doc d
    loop ph mh qs pc doc     (DocGroup        _ d) = loop ph mh qs pc doc d
    loop ph mh qs pc _       (Document        t d) = loop ph mh qs pc (Just t) d
    loop ph mh qs pc (Just t) Action               = Just . PathDoc (ph End) $ mh [MethodDoc (qs []) (pc []) t]
    loop _  _  _  _  Nothing  Action               = Nothing

    dropNext (DocPath         _ d) = d
    dropNext (DocRoot           d) = d
    dropNext (DocFetch    _ _ _ d) = d
    dropNext (DocRest       _ _ d) = d
    dropNext (DocAny            d) = d
    dropNext (DocDropNext       d) = dropNext d
    dropNext (DocMethod       _ d) = d
    dropNext (DocQuery  _ _ _ _ d) = d
    dropNext (DocPrecondition _ d) = d
    dropNext (DocGroup        _ d) = d
    dropNext (Document        _ d) = d
    dropNext Action                = Action

mergePathDoc :: [PathDoc] -> [PathDoc]
mergePathDoc [] = []
mergePathDoc (pd:pds) = merge (filter (same pd) pds) : mergePathDoc (filter (not . same pd) pds)
  where
    same           = (==) `on` path
    merge pds' = PathDoc (path pd) (mergeMethods $ methods pd ++ concatMap methods pds')

mergeMethods :: [(Method, [MethodDoc])] -> [(Method, [MethodDoc])]
mergeMethods [] = []
mergeMethods (m:ms) = merge (filter (same m) ms) : mergeMethods (filter (not . same m) ms)
  where
    same = (==) `on` fst
    merge ms' = (fst m, snd m ++ concatMap snd ms')


docsToDocuments :: [Doc] -> Documents
docsToDocuments doc =
    let gds = mapMaybe docToDocument doc
        ngs = mergePathDoc . map snd $ filter ((Nothing ==) . fst)   gds
        gs  = map upGroup . groupBy ((==) `on` fst) $ mapMaybe trav gds
    in Documents ngs gs
  where
    upGroup ((g,d):ig) = (g, mergePathDoc $ d : map snd ig)
    upGroup []         = error "docsToDocuments: unknown error."

    trav (Nothing, _) = Nothing
    trav (Just a,  b) = Just (a, b)
