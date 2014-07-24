{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Apiary.Document.Html where

import Language.Haskell.TH

import Data.Monoid
import Data.Default.Class

import Data.Apiary.Param
import Data.Apiary.Method
import Data.Apiary.Document

import Text.Blaze.Html
import Text.Blaze.Internal(attribute)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


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

    noDesc = H.span "no description" ! A.class_ "no-description"

    query (QueryDoc p s q t) = H.tr . mconcat $
        [ H.td (toHtml $ T.decodeUtf8 p)
        , H.td (toHtml $ strategyInfo s)
        , H.td (htmlQR q)
        , H.td $ maybe noDesc id t
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
