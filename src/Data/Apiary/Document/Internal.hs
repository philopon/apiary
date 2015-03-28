{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Data.Apiary.Document.Internal
    ( Doc(..)
    , Documents(..)
    , PathDoc(..)
    , QueryDoc(..)
    , MethodDoc(..)
    , Route(..)
    , docsToDocuments
    ) where

import Data.Typeable(TypeRep)
import Data.Maybe(mapMaybe)
import Data.List(groupBy)
import Data.Function(on)

import Data.Apiary.Param(StrategyRep, QueryRep)
import Data.Apiary.Method(Method)

import Data.Apiary.Html(Html)
import qualified Data.Text as T
import qualified Data.ByteString as S

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
    | DocAccept S.ByteString Doc
    | DocGroup  T.Text       Doc
    | Document  Html         Doc
    | Action

--------------------------------------------------------------------------------

data Route
    = Path  T.Text                      Route
    | Fetch T.Text TypeRep (Maybe Html) Route
    | Rest  T.Text (Maybe Html) -- ^ \*\* with name
    | Any -- ^ \*\* without name
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
    , methods :: [(Maybe Method, [MethodDoc])]
    }

-- | query parameters document
data QueryDoc = QueryDoc
    { queryName     :: T.Text
    , queryStrategy :: StrategyRep
    , queryRep      :: QueryRep
    , queryDocument :: Maybe Html
    }

data MethodDoc = MethodDoc
    { queries       :: [QueryDoc]
    , preconditions :: [Html]
    , accept        :: Maybe S.ByteString
    , document      :: Maybe Html
    }

--------------------------------------------------------------------------------

data ToDocumentState = ToDocumentState
    { toDocumentPath      :: Route -> Route
    , toDocumentMethodDoc :: [MethodDoc] -> [(Maybe Method, [MethodDoc])]
    , toDocumentQueries   :: [QueryDoc] -> [QueryDoc]
    , toDocumentPreconds  :: [Html] -> [Html]
    , toDocumentAccept    :: Maybe S.ByteString
    , toDocumentDocument  :: Maybe Html
    }

initialToDocumentState :: ToDocumentState
initialToDocumentState = ToDocumentState id (\md -> [(Nothing, md)]) id id Nothing Nothing

docToDocument :: Doc -> Maybe (Maybe T.Text, PathDoc)
docToDocument = \case
    (DocGroup "" d') -> (Nothing,) `fmap` loop initialToDocumentState d'
    (DocGroup g  d') -> (Just  g,) `fmap` loop initialToDocumentState d'
    d'               -> (Nothing,) `fmap` loop initialToDocumentState d'
  where
    loop st (DocDropNext       d) = loop st (dropNext d)
    loop st (DocPath         p d) = loop st { toDocumentPath = toDocumentPath st . Path p } d
    loop st (DocRoot           d) = loop st { toDocumentPath = const $ Path "" End } d
    loop st (DocFetch    k t h d) = loop st { toDocumentPath = toDocumentPath st . Fetch k t h } d
    loop st (DocRest     k   h d) = loop st { toDocumentPath = toDocumentPath st . const (Rest k h) } d
    loop st (DocAny            d) = loop st { toDocumentPath = toDocumentPath st . const Any } d
    loop st (DocMethod       m d) = loop st { toDocumentMethodDoc = (\md -> [(Just m, md)]) } d
    loop st (DocQuery  p s q t d) = loop st { toDocumentQueries = toDocumentQueries st . (QueryDoc p s q t:) } d
    loop st (DocPrecondition h d) = loop st { toDocumentPreconds = toDocumentPreconds st . (h:) } d
    loop st (DocGroup        _ d) = loop st d
    loop st (DocAccept       a d) = loop st { toDocumentAccept = Just a } d
    loop st (Document        t d) = loop st { toDocumentDocument = Just t} d
    loop st Action                = Just . PathDoc (toDocumentPath st End) $ toDocumentMethodDoc st
        [MethodDoc (toDocumentQueries st []) (toDocumentPreconds st []) (toDocumentAccept st) (toDocumentDocument st)]

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
    dropNext (DocAccept       _ d) = d
    dropNext (Document        _ d) = d
    dropNext Action                = Action

mergePathDoc :: [PathDoc] -> [PathDoc]
mergePathDoc [] = []
mergePathDoc (pd:pds) = merge (filter (same pd) pds) : mergePathDoc (filter (not . same pd) pds)
  where
    same           = (==) `on` path
    merge pds' = PathDoc (path pd) (mergeMethods $ methods pd ++ concatMap methods pds')

mergeMethods :: [(Maybe Method, [MethodDoc])] -> [(Maybe Method, [MethodDoc])]
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
