{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Apiary.Documentation
    ( DocumentConfig(..)
    , documentation
    ) where

import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad.Apiary.Internal(ApiaryT, action)
import Control.Monad.Apiary.Action.Internal(getDocuments, lazyText)
import Control.Monad.Apiary.Filter(method, accept)

import Data.Apiary.Html(toLazyText)
import Data.Apiary.Method(Method(..))
import Data.Apiary.Document.Html(DocumentConfig(..), documentToHtml, defaultTemplate)


-- | auto generated document.
documentation :: (MonadIO m, Monad actM) => DocumentConfig -> ApiaryT exts prms actM m ()
documentation cfg = method GET $ do
    template <- liftIO defaultTemplate
    accept "text/html" . action $ do
        d <- getDocuments
        lazyText . toLazyText $ documentToHtml cfg template d
