{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Apiary.Document.Html
    ( documentToHtml
    -- * config
    , DocumentConfig(..)

    -- * template
    , parseTemplateFile
    ) where

import Data.Monoid
import qualified Language.Haskell.TH as TH
import qualified Paths_apiary as Paths
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Read as R
import qualified Data.ByteString.Builder as B
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import Data.Default.Class

import Data.Apiary.Document.Internal(Documents, Desc(..))
import Data.Apiary.Document.JSON(documentsToJSON)
import Text.Blaze.JSON(JSON)
import qualified Text.Blaze.JSON as JSON
import qualified Text.Blaze.JSON.Internal as JSONI

templateFile :: FilePath
templateFile = $(TH.litE . TH.stringL =<< TH.runIO (Paths.getDataFileName "template.bin"))

data DocumentConfig = forall d. Desc d => DocumentConfig
    { documentTemplate    :: FilePath
    , documentTitle       :: T.Text
    , documentDescription :: d
    }

instance Default DocumentConfig where
    def = DocumentConfig
        { documentTemplate    = templateFile
        , documentTitle       = "API documentation" 
        , documentDescription = ()
        }

documentJSON :: DocumentConfig -> Documents -> JSON
documentJSON DocumentConfig{..} doc = JSON.object $
    (:) ("title", JSON.text documentTitle) $
    maybe id (\d -> (:) ("description", d)) (toDesc documentDescription) $
    [("data", documentsToJSON doc)]

newtype Template = Template (JSON -> B.Builder)

parseTemplate :: L.ByteString -> Either String Template
parseTemplate str = either (\(_,_,m) -> Left m) (\(_,_,a) -> Right a) .
    flip Bin.runGetOrFail str $ do
        len <- Bin.get
        pre <- Bin.getLazyByteString len
        suf <- Bin.getRemainingLazyByteString
        return . Template $ \dat ->
            B.lazyByteString pre <> JSON.toBuilder def dat <> B.lazyByteString suf

parseTemplateFile :: FilePath -> IO Template
parseTemplateFile file = L.readFile file >>=
    either fail return . parseTemplate

documentToBuilder :: DocumentConfig -> Template -> Documents -> B.Builder
documentToBuilder cfg (Template apply) = apply . documentJSON cfg
