{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Apiary.Document where

import Language.Haskell.TH
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import Data.Typeable
import Data.Maybe
import Data.Apiary.Param
import qualified Network.HTTP.Types as HT
import Text.Blaze.Html
import Text.Blaze.Internal(attribute)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid
import Data.Default.Class
import Data.List
import Data.Function

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
    | DocPrecondition Html   Doc
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
    { queries       :: [QueryDoc]
    , preconditions :: [Html]
    , document      :: T.Text
    }

docToDocument :: Doc -> Maybe (Maybe T.Text, PathDoc)
docToDocument = \case
    (DocGroup g d') -> (Just  g,) <$> loop id (\md -> [("ANY", md)]) id id d'
    d'              -> (Nothing,) <$> loop id (\md -> [("ANY", md)]) id id d'
  where
    loop ph mh qs pc (DocPath         t d) = loop (ph . Path t) mh qs pc d
    loop _  mh qs pc (DocRoot           d) = loop (const $ Path "" End) mh qs pc d
    loop ph mh qs pc (DocFetch      t h d) = loop (ph . Fetch t h) mh qs pc d
    loop ph _  qs pc (DocMethod       m d) = loop ph (\md -> [(m, md)]) qs pc d
    loop ph mh qs pc (DocQuery  p s q t d) = loop ph mh (qs . (QueryDoc p s q t:)) pc d
    loop ph mh qs pc (DocGroup        _ d) = loop ph mh qs pc d
    loop ph mh qs pc (DocPrecondition h d) = loop ph mh qs (pc . (h:)) d
    loop ph mh qs pc (Document   (Just t)) = Just . PathDoc (ph End) $ mh (MethodDoc (qs []) (pc []) t)
    loop _  _  _  _  (Document    Nothing) = Nothing

mergeMethod :: [PathDoc] -> [PathDoc]
mergeMethod [] = []
mergeMethod (pd:pds) = merge pd (filter (same pd) pds) : mergeMethod (filter (not . same pd) pds)
  where
    same = (==) `on` path
    merge pd' pds' = PathDoc (path pd') (methods pd' ++ concatMap methods pds')

docsToDocuments :: [Doc] -> Documents
docsToDocuments doc =
    let gds = mapMaybe docToDocument doc
        ngs = mergeMethod . map snd $ filter ((Nothing ==) . fst)   gds
        gs  = map upGroup . groupBy ((==) `on` fst) $ mapMaybe trav gds
    in Documents ngs gs
  where
    upGroup ((g,d):ig) = (g, mergeMethod $ d : map snd ig)
    upGroup []         = error "docsToDocuments: unknown error."

    trav (Nothing, _) = Nothing
    trav (Just a,  b) = Just (a, b)

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
            else H.table ! A.class_ "table table-condensed route-parameters" $
                 H.tr (mconcat 
                    [ H.th ! A.class_ "col-sm-1 com-md-1" $ "#"
                    , H.th ! A.class_ "col-sm-1 com-md-1" $ "type"
                    , H.th "description"
                    ]) <> mconcat p
        )


data DefaultDocumentConfig = DefaultDocumentConfig
    { documentTitle       :: T.Text
    , documentDescription :: Maybe Html
    }

instance Default DefaultDocumentConfig where
    def = DefaultDocumentConfig "API documentation" Nothing

defaultDocumentToHtml :: DefaultDocumentConfig -> Documents -> Html
defaultDocumentToHtml DefaultDocumentConfig{..} docs =
    H.docTypeHtml $ H.head headH <> H.body body <> footer
  where
    css u = H.link ! A.rel "stylesheet" ! A.href u
    js  u = H.script ! A.src u $ mempty

    headH = mconcat 
        [ H.title (toHtml documentTitle)
        , css "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
        , js  "//code.jquery.com/jquery-2.1.1.min.js"
        , js  "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
        , $(runIO (readFile "static/jquery.cookie-1.4.1.min.js") >>= \c -> [|H.script $ preEscapedToHtml (c::String)|])
        , $(runIO (readFile "static/api-documentation.min.js")   >>= \c -> [|H.script $ preEscapedToHtml (c::String)|])
        , $(runIO (readFile "static/api-documentation.min.css")     >>= \c -> [|H.style  $ preEscapedToHtml (c::String)|])
        ]

    htmlQR (Strict   r) = toHtml (show r)
    htmlQR (Nullable r) = toHtml (show r ++ "?")
    htmlQR  Check       = toHtml ("check" :: T.Text)

    query (QueryDoc p s q t) = H.tr . mconcat $
        [ H.td (toHtml $ T.decodeUtf8 p)
        , H.td (toHtml $ strategyInfo s)
        , H.td (htmlQR q)
        , H.td t
        ]

    mcMap f = mconcat . map f

    queriesH [] = mempty
    queriesH qs =
        H.div ! A.class_ "col-sm-offset-1 col-md-offset-1" $
        H.table ! A.class_ "table table-condensed" $ mconcat
        [ H.caption "Query parameters"
        , H.tr $ mconcat [ H.th ! A.class_ "col-sm-1 col-md-1" $ "name"
                         , H.th ! A.class_ "col-sm-1 col-md-1" $ "num"
                         , H.th ! A.class_ "col-sm-1 col-md-1" $ "type"
                         , H.th "description"
                         ]
        , mcMap query qs
        ]

    preconds [] = mempty
    preconds p =
        H.div ! A.class_ "well well-sm col-sm-offset-1 col-md-offset-1" $ mconcat
            [ H.p "Preconditions:"
            , H.ul $ mcMap (\h -> H.li h) p
            ]

    method (m, MethodDoc qs pc d) = 
        H.div ! A.class_ "method" $ mconcat
        [ H.h4 . toHtml $ T.decodeUtf8 m
        , preconds pc
        , H.div ! A.class_ "col-sm-offset-1 col-md-offset-1" $ H.p (toHtml d)
        , queriesH qs
        ]

    dataToggle = attribute "data-toggle" " data-toggle=\""
    dataTarget = attribute "data-target" " data-target=\""

    pathH i (PathDoc r ms) =
        let (route, rdoc) = routeToHtml r
        in H.div ! A.class_ "panel panel-default" $ mconcat
            [ H.div ! A.class_ "panel-heading clearfix"
                ! dataToggle "collapse" ! dataTarget (toValue $ "#collapse-" ++ show (i::Int)) $ mconcat
                [ H.h3 ! A.class_ "panel-title pull-left" $ route
                , H.div ! A.class_ "methods" $
                    mcMap ((\m -> H.div ! A.class_ "pull-right" $ toHtml (T.decodeUtf8 m)) . fst) (reverse ms)
                ]
            , H.div ! A.id (toValue $ "collapse-" ++ show i) ! A.class_ "panel-collapse collapse" $
                rdoc <> (H.div ! A.class_ "panel-body" $ mcMap method ms)
            ]

    groupH i (g, p) =
        let (i', gs) = mapAccumL (\ii a -> (succ ii, pathH ii a)) i p
        in (i', H.div ! A.id (toValue $ T.append "group-" g) $ mconcat [H.h2 $ toHtml g, mconcat gs])

    doc (Documents n g) =
        let (i, ng) = mapAccumL (\i' a -> (succ i', pathH  i' a)) (0::Int) n
            (_, gs) = mapAccumL groupH i g
        in (H.div ! A.id "no-group") (mconcat ng) <> mconcat gs

    body  = H.div ! A.class_ "container" $ mconcat
        [ H.div ! A.class_ "page-header" $ H.h1 (toHtml documentTitle)
        , maybe mempty (H.div ! A.class_ "description") documentDescription
        , doc docs
        ]

    footer = H.footer $ mconcat
        [ "This API documentation generated by "
        , H.a ! A.href "https://github.com/philopon/apiary" $ "apiary"
        , " web framework."
        ]
