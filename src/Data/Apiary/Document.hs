{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Apiary.Document where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import Data.Typeable
import Data.Maybe
import Data.Apiary.Param
import qualified Network.HTTP.Types as HT
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid
import Data.List
import Data.Function
import Data.Traversable

data StrategyRep = StrategyRep
    { strategyInfo :: T.Text }
    deriving (Show, Eq)

data Doc
    = DocPath   T.Text       Doc
    | DocRoot                Doc
    | DocFetch  TypeRep (Maybe Html) Doc
    | DocMethod HT.Method    Doc
    | DocQuery  S.ByteString StrategyRep QueryRep Html Doc
    | DocGroup  T.Text       Doc
    | Document  (Maybe T.Text)

data Route
    = Path  T.Text               Route
    | Fetch TypeRep (Maybe Html) Route
    | End

instance Eq Route where
    Path  a   d == Path  b   d' = a == b && d == d'
    Fetch a _ d == Fetch b _ d' = a == b && d == d'
    End         == End          = True
    _           == _            = False

data Documents = Documents
    { noGroup :: [PathDoc]
    , groups  :: [(T.Text, [PathDoc])]
    }

data PathDoc = PathDoc
    { path    :: Route
    , methods :: [(HT.Method, MethodDoc)]
    }

data QueryDoc = QueryDoc
    { queryName     :: S.ByteString
    , queryStrategy :: StrategyRep
    , queryRep      :: QueryRep
    , queryDocument :: Html
    }

data MethodDoc = MethodDoc
    { queries  :: [QueryDoc]
    , document :: T.Text
    }

docToDocument :: Doc -> Maybe (PathDoc, Maybe T.Text)
docToDocument = \case
    (DocGroup g d') -> (,Just  g) <$> loop id (\md -> [("ANY", md)]) id d'
    d'              -> (,Nothing) <$> loop id (\md -> [("ANY", md)]) id d'
  where
    loop ph mh qs (DocPath        t d)  = loop (ph . Path t) mh qs d
    loop _  mh qs (DocRoot          d)  = loop (const $ Path "" End) mh qs d
    loop ph mh qs (DocFetch     t h d)  = loop (ph . Fetch t h) mh qs d
    loop ph _  qs (DocMethod      m d)  = loop ph (\md -> [(m, md)]) qs d
    loop ph mh qs (DocQuery p s q t d)  = loop ph mh (qs . (QueryDoc p s q t:)) d
    loop ph mh qs (DocGroup       _ d)  = loop ph mh qs d
    loop ph mh qs (Document   (Just t)) = Just . PathDoc (ph End) $ mh (MethodDoc (qs []) t)
    loop _  _  _  (Document   Nothing)  = Nothing

mergeMethod :: [PathDoc] -> [PathDoc]
mergeMethod [] = []
mergeMethod (pd:pds) = merge pd (filter (same pd) pds) : mergeMethod (filter (not . same pd) pds)
  where
    same = (==) `on` path
    merge pd' pds' = PathDoc (path pd') (methods pd' ++ concatMap methods pds')

docsToDocuments :: [Doc] -> Documents
docsToDocuments doc =
    let gds = mapMaybe docToDocument doc
        ngs = mergeMethod . map fst $ filter ((Nothing ==) . snd) gds
        gs  = map upGroup . groupBy ((==) `on` snd) $ mapMaybe sequenceA gds
    in Documents ngs gs
  where
    upGroup ((d,g):ig) = (g, mergeMethod $ d : map fst ig)
    upGroup []         = error "docsToDocuments: unknown error."

routeToHtml :: Route -> (Html, Html)
routeToHtml = loop (1::Int) mempty []
  where
    sp = H.span "/" ! A.class_ "splitter"
    loop i r p (Path s d)          = loop i (r <> sp <> H.span (toHtml s) ! A.class_ "path") p d
    loop i r p (Fetch t Nothing d) = 
        loop i (r <> sp <> H.span (toHtml $ ':' : show t) ! A.class_ "fetch") p d
    loop i r p (Fetch t (Just h) d) = 
        loop (succ i) (r <> sp <> H.span (toHtml (':' : show t) <> H.sup (toHtml i)) ! A.class_ "fetch")
            (p <> [H.tr $ H.td (toHtml i) <> H.td (toHtml $ show t) <> H.td h]) d
    loop _ r p End =
        (r, if null p
            then mempty
            else H.table ! A.class_ "table table-condensed col-sm-offset-1 col-md-offset-1" $
                 H.caption "Route Parameters" <>
                 H.tr (H.th "#" <> H.th "type" <> H.th "description") <>
                 mconcat p
        )


defaultDocumentToHtml :: Documents -> Html
defaultDocumentToHtml docs = H.docTypeHtml $ H.html $ H.head headH <> H.body body
  where
    css u = H.link ! A.rel "stylesheet" ! A.href u
    headH = H.title "API document" <>
        css "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"

    htmlQR (Strict   r) = toHtml (show r)
    htmlQR (Nullable r) = toHtml (show r ++ "?")
    htmlQR  Check       = toHtml ("check" :: T.Text)
    query (QueryDoc p s q t) = H.tr $
        H.td (toHtml $ T.decodeUtf8 p) <> H.td (toHtml $ strategyInfo s) <> H.td (htmlQR q) <> H.td t

    queriesH []    = mempty
    queriesH qs    =
        H.table ! A.class_ "table table-condensed col-sm-offset-1 col-md-offset-1" $
        H.caption "Query Parameters" <>
        H.tr (H.th "name" <> H.th "num" <> H.th "type" <> H.th "description") <>
        mconcat (map query qs)

    method (m, MethodDoc qs d) = 
        H.div ! A.class_ "col-sm-offset-1 col-md-offset-1" $
        H.h4 (toHtml $ T.decodeUtf8 m) <> queriesH qs <> 
        (H.p ! A.class_ "col-sm-offset-1 col-md-offset-1") (toHtml d)

    pathH (PathDoc r ms) =
        let (route, rdoc) = routeToHtml r
            in H.div $ H.h3 route <> rdoc <> (H.div $ mconcat (map method ms))

    groupH (g, p) = H.div $ H.h2 (toHtml g) <> (mconcat $ map pathH p)

    doc (Documents n g) = H.div (mconcat $ map pathH n) <> mconcat (map groupH g)

    body  = H.div ! A.class_ "container" $
        H.h1 "API document" <> doc docs
