{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Apiary.Document where

import Language.Haskell.TH

import Control.Applicative

import Data.Typeable
import Data.Maybe
import Data.Monoid
import Data.Default.Class
import Data.List
import Data.Function

import Data.Apiary.Param
import Data.Apiary.Method

import Text.Blaze.Html
import Text.Blaze.Internal(attribute)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data StrategyRep = StrategyRep
    { strategyInfo :: T.Text }
    deriving (Show, Eq)

data Doc
    = DocPath   T.Text       Doc
    | DocRoot                Doc
    | DocFetch  TypeRep (Maybe Html) Doc
    | DocDropNext            Doc

    | DocMethod Method       Doc
    | DocQuery  S.ByteString StrategyRep QueryRep Html Doc
    | DocPrecondition Html   Doc
    | DocGroup  T.Text       Doc
    | Document  T.Text       Doc
    | Action

--------------------------------------------------------------------------------

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
    , methods :: [(Method, [MethodDoc])]
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
    loop ph mh qs pc doc     (DocFetch      t h d) = loop (ph . Fetch t h) mh qs pc doc d
    loop ph _  qs pc doc     (DocMethod       m d) = loop ph (\md -> [(m, md)]) qs pc doc d
    loop ph mh qs pc doc     (DocQuery  p s q t d) = loop ph mh (qs . (QueryDoc p s q t:)) pc doc d
    loop ph mh qs pc doc     (DocPrecondition h d) = loop ph mh qs (pc . (h:)) doc d
    loop ph mh qs pc doc     (DocGroup        _ d) = loop ph mh qs pc doc d
    loop ph mh qs pc _       (Document        t d) = loop ph mh qs pc (Just t) d
    loop ph mh qs pc (Just t) Action               = Just . PathDoc (ph End) $ mh [MethodDoc (qs []) (pc []) t]
    loop _  _  _  _  Nothing  Action               = Nothing

    dropNext (DocPath         _ d) = d
    dropNext (DocRoot           d) = d
    dropNext (DocFetch      _ _ d) = d
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

-------------------------------------------------------------------------------

routeToHtml :: Route -> (T.Text, Html, Html)
routeToHtml = loop (1::Int) "" mempty []
  where
    sp = H.span "/" ! A.class_ "splitter"
    loop i e r p (Path s d)          = loop i (T.concat [e, "/", s]) (r <> sp <> H.span (toHtml s) ! A.class_ "path") p d
    loop i e r p (Fetch t Nothing d) = 
        loop (succ i) (T.concat [e, "/:", T.pack $ show t]) (r <> sp <> rpHtml (toHtml $ show t) i) p d
    loop i e r p (Fetch t (Just h) d) = 
        loop (succ i) (T.concat [e, "/:", T.pack $ show t]) (r <> sp <> rpHtml (toHtml $ show t) i)
            (p <> [H.tr $ H.td (toHtml i) <> H.td (toHtml $ show t) <> H.td h]) d
    loop _ e r p End =
        (e, r
        , if null p
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
    , documentUseCDN      :: Bool
    }

instance Default DefaultDocumentConfig where
    def = DefaultDocumentConfig "API documentation" Nothing True

defaultDocumentToHtml :: DefaultDocumentConfig -> Documents -> Html
defaultDocumentToHtml DefaultDocumentConfig{..} docs =
    H.docTypeHtml $ H.head headH <> H.body body <> footer
  where
    css u = H.link ! A.rel "stylesheet" ! A.href u
    js  u = H.script ! A.src u $ mempty
    dataToggle = attribute "data-toggle" " data-toggle=\""
    dataTarget = attribute "data-target" " data-target=\""

    mcMap f = mconcat . map f

    cdns = mconcat
        [ css "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
        , js  "//code.jquery.com/jquery-2.1.1.min.js"
        , js  "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
        ]

    embeds = $( do
        let embed f p = runIO (readFile p) >>= \c -> [|$(varE f) $ preEscapedToHtml (c :: String)|]
        [| mconcat 
            [ $(embed 'H.style  "static/bootstrap.min.css")
            , $(embed 'H.script "static/jquery-2.1.1.min.js")
            , $(embed 'H.script "static/bootstrap.min.js")
            ]
         |])

    headH = mconcat 
        [ H.title (toHtml documentTitle)
        , if documentUseCDN then cdns else embeds
        , $(runIO (readFile "static/jquery.cookie-1.4.1.min.js") >>= \c -> [|H.script $ preEscapedToHtml (c::String)|])
        , $(runIO (readFile "static/api-documentation.min.js")   >>= \c -> [|H.script $ preEscapedToHtml (c::String)|])
        , $(runIO (readFile "static/api-documentation.min.css")  >>= \c -> [|H.style  $ preEscapedToHtml (c::String)|])
        ]

    htmlQR (Strict   r) = toHtml (show r)
    htmlQR (Nullable r) = H.span (toHtml (show r) <> "?") ! A.title (toValue (show r) <> "(nullable)")
    htmlQR  Check       = "check"

    query (QueryDoc p s q t) = H.tr . mconcat $
        [ H.td (toHtml $ T.decodeUtf8 p)
        , H.td (toHtml $ strategyInfo s)
        , H.td (htmlQR q)
        , H.td t
        ]

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
        H.div ! A.class_ "well well-sm precondition" $ mconcat
            [ H.p "Preconditions:"
            , H.ul $ mcMap (\h -> H.li h) p
            ]

    method (m, ms) = H.div ! A.class_ "method" $ mconcat
        [ H.h4 . toHtml $ T.decodeUtf8 $ renderMethod m
        , mcMap action ms
        ]

    action (MethodDoc qs pc d) = H.div ! A.class_ "action col-sm-offset-1 col-md-offset-1" $ mconcat
        [ preconds pc
        , H.div $ H.p (toHtml d)
        , queriesH qs
        ]

    pathH grp (PathDoc r ms) =
        let (idnt, route, rdoc) = routeToHtml r
        in H.div ! A.class_ "panel panel-default" $ mconcat
            [ H.div ! A.class_ "panel-heading clearfix"
                ! dataToggle "collapse" ! dataTarget (toValue $ T.concat ["[id='collapse-", grp, "-", idnt, "']"]) $ mconcat
                [ H.h3 ! A.class_ "panel-title pull-left" $ route
                , H.div ! A.class_ "methods" $
                    mcMap ((\m -> H.div ! A.class_ "pull-right" $ toHtml (T.decodeUtf8 m)) . renderMethod . fst) (reverse ms)
                ]
            , H.div ! A.id (toValue $ T.concat ["collapse-", grp, "-", idnt]) ! A.class_ "panel-collapse collapse" $
                rdoc <> (H.div ! A.class_ "panel-body" $ mcMap method ms)
            ]

    groupH (g, p) =
        let gs = mcMap (pathH g) p
        in H.div ! A.id (toValue $ T.append "group-" g) $ mconcat [H.h2 $ toHtml g, gs]

    doc (Documents n g) =
        let ng = mcMap (pathH "") n
            gs = mcMap groupH g
        in (H.div ! A.id "no-group") ng <> gs

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

-- | construct Html as route parameter. since 0.13.0.
rpHtml :: Html -> Int -> Html
rpHtml s i = H.span (":" <> s <> H.sup (toHtml i)) ! A.class_ "fetch"
