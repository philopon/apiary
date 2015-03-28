{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Data.Apiary.Html (Html, toLazyText, toHtml, preEscaped) where

import Data.String(IsString(..))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import qualified Data.Text.Lazy as T

newtype Html = Html { unHtml :: T.Text }

instance Show Html where
    show = show . unHtml

instance IsString Html where
    fromString = Html . escapeHtml . T.pack

instance Monoid Html where
    mempty  = Html T.empty
    mappend (Html a) (Html b) = Html (a `T.append` b)

escapeHtml :: T.Text -> T.Text
escapeHtml = T.concatMap esc
  where
    esc  '<' = "&lt;"
    esc  '>' = "&gt;"
    esc  '&' = "&amp;"
    esc  '"' = "&quot;"
    esc '\'' = "&#39;"
    esc c   = T.singleton c

toHtml :: T.Text -> Html
toHtml = Html . escapeHtml

preEscaped :: T.Text -> Html
preEscaped = Html

toLazyText :: Html -> T.Text
toLazyText = unHtml
