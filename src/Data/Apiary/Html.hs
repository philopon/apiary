{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Data.Apiary.Html (Html, toLazyText, toHtml, preEscaped) where

import Data.String(IsString(..))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import qualified Data.Text.Lazy as L

newtype Html = Html { unHtml :: L.Text }

instance Show Html where
    show = show . unHtml

instance IsString Html where
    fromString = Html . escapeHtml . L.pack

instance Monoid Html where
    mempty  = Html L.empty
    mappend (Html a) (Html b) = Html (a `L.append` b)

escapeHtml :: L.Text -> L.Text
escapeHtml = L.concatMap esc
  where
    esc  '<' = "&lt;"
    esc  '>' = "&gt;"
    esc  '&' = "&amp;"
    esc  '"' = "&quot;"
    esc '\'' = "&#39;"
    esc c   = L.singleton c

toHtml :: L.Text -> Html
toHtml = Html . escapeHtml

preEscaped :: L.Text -> Html
preEscaped = Html

toLazyText :: Html -> L.Text
toLazyText = unHtml
