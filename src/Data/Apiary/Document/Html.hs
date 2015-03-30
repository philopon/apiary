{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Apiary.Document.Html
    ( documentToHtml
    -- * config
    , DocumentConfig(..)

    -- * template
    , parseTemplate
    , parseTemplateFile
    , defaultTemplate
    ) where

import qualified Language.Haskell.TH as TH
import qualified Paths_apiary as Paths
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Read as R
import qualified Data.Text.Lazy.Builder as B
import Data.Default.Class

import Data.Apiary.Html
import Data.Apiary.Document(Documents)
import qualified Data.Apiary.Document.JSON as JSON

templateFile :: FilePath
templateFile = $(TH.litE . TH.stringL =<< TH.runIO (Paths.getDataFileName "template.html"))

data DocumentConfig = DocumentConfig
    { documentTitle       :: T.Text
    , documentDescription :: Maybe Html
    }

instance Default DocumentConfig where
    def = DocumentConfig
        { documentTitle       = "API documentation" 
        , documentDescription = Nothing
        }

documentJSON :: DocumentConfig -> Documents -> JSON.JSON
documentJSON DocumentConfig{..} doc = JSON.object $
    (:) ("title", JSON.string' documentTitle) $
    maybe id (\d -> (:) ("description",  JSON.string $ toLazyText d)) documentDescription $
    [("data", JSON.documentsToJSON doc)]

data Template = Template L.Text L.Text

parseTemplate :: L.Text -> Maybe Template
parseTemplate str =
    let (pLen, body) = (fmap L.tail . L.break (== '\n')) str
    in case R.decimal pLen of
        Right (len, "") -> Just . uncurry Template $ L.splitAt len body
        _               -> Nothing

parseTemplateFile :: FilePath -> IO Template
parseTemplateFile file = L.readFile file >>=
    maybe (fail "Data.Apiary.Document.Html.parseTemplate: parse failed.") return . parseTemplate

defaultTemplate :: IO Template
defaultTemplate = parseTemplateFile templateFile

documentToHtml :: DocumentConfig -> Template -> Documents -> Html
documentToHtml cfg (Template pre suf) doc =
    let json = B.toLazyText . JSON.unJSON $ documentJSON cfg doc
    in preEscaped $ pre `L.append` json `L.append` suf
