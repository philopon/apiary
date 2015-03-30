{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Apiary.Document.JSON (documentsToJSON) where

import Data.Monoid((<>), mconcat)
import Data.List(intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

import Data.Apiary.Param(StrategyRep(..), QueryRep(..))
import Data.Apiary.Document
import Data.Apiary.Method(Method, renderMethod)
import Data.Apiary.Html

import Unsafe.Coerce

newtype JSON = JSON { unJSON :: B.Builder } deriving Show

unJSONList :: [JSON] -> [B.Builder]
unJSONList = unsafeCoerce

escapeJSON' :: T.Text -> T.Text
escapeJSON' = T.concatMap escape
  where
    escape c | '\x0' <= c && c < '\x20' = escapeU c
             | c `elem` toEscapeChars   = escapeU c
             | otherwise = T.singleton c

    toEscapeChars = ['<', '>', '&', '\'', '"', '/']

    hexDigit c = toEnum $ if c < 10 then c + 48 else c + 87

    escapeU c = case fromEnum c `quotRem` 0x10 of
        (d1, d0) -> "\\u00" `T.snoc` hexDigit d1 `T.snoc` hexDigit d0

escapeJSON :: L.Text -> L.Text
escapeJSON = L.foldrChunks (\t a -> L.fromStrict (escapeJSON' t) `L.append` a) L.empty

jsonString' :: T.Text -> JSON
jsonString' s = JSON $ B.singleton '"' <> B.fromText (escapeJSON' s) <> B.singleton '"'

jsonString :: L.Text -> JSON
jsonString s = JSON $ B.singleton '"' <> B.fromLazyText (escapeJSON s) <> B.singleton '"'

jsonObject :: [(T.Text, JSON)] -> JSON
jsonObject = JSON . obj . mconcat . intersperse (B.singleton ',') .
    map (\(k,JSON v) -> unJSON (jsonString' k) <> B.singleton ':' <> v)
  where
    obj b = B.singleton '{' <> b <> B.singleton '}'

jsonArray :: [JSON] -> JSON
jsonArray = JSON . ary . mconcat . intersperse (B.singleton ',') . unJSONList
  where
    ary b = B.singleton '[' <> b <> B.singleton ']'

routeToJSON' :: Route -> [JSON]
routeToJSON' (Path p n) = jsonObject obj : routeToJSON' n
  where
    obj = [ ("tag",  jsonString' "path")
          , ("path", jsonString' p)
          ]
routeToJSON' (Fetch k t md n) = jsonObject obj : routeToJSON' n
  where
    obj = (:) ("tag",  jsonString' "fetch") $
          (:) ("name", jsonString' k) $
          (:) ("type", jsonString' . T.pack $ show t) $
          maybe id (\d -> (:) ("doc", jsonString $ toLazyText d)) md $
          []
routeToJSON' (Rest k md) = (:[]) $ jsonObject $
    (:) ("tag", jsonString' "rest") $
    (:) ("name", jsonString' k) $
    maybe id (\d -> (:) ("doc", jsonString $ toLazyText d)) md $
    []
routeToJSON' Any = [jsonObject [ ("tag", jsonString' "rest") ]]
routeToJSON' End = []

routeToJSON :: Route -> JSON
routeToJSON = jsonArray . routeToJSON'

queryDocToJSON :: QueryDoc -> JSON
queryDocToJSON QueryDoc{..} = jsonObject $
    (:) ("name", jsonString' queryName) $
    (:) ("strategy", jsonString' $ strategyInfo queryStrategy) $
    (:) ("query", jsonString' query) $
    maybe id (\t -> (:) ("type", jsonString' . T.pack $ show t)) queryType $
    maybe id (\d -> (:) ("doc", jsonString $ toLazyText d)) queryDocument $
    []
  where
    query = case queryRep of
        Strict   _ -> "strict"
        Nullable _ -> "nullable"
        Check      -> "check"
        NoValue    -> "noValue"

    queryType = case queryRep of
        Strict   t -> Just t
        Nullable t -> Just t
        _          -> Nothing

methodDocToJSON :: MethodDoc -> JSON
methodDocToJSON MethodDoc{..} = jsonObject $
    (:) ("queries", jsonArray $ map queryDocToJSON queries) $
    (:) ("preconditions", jsonArray $ map (jsonString . toLazyText) preconditions) $
    maybe id (\a -> (:) ("accept", jsonString' $ T.decodeUtf8 a)) accept $
    maybe id (\d -> (:) ("doc", jsonString $ toLazyText d)) document []

methodToJSON :: Maybe Method -> [MethodDoc] -> JSON
methodToJSON mbm md = jsonObject $
    maybe id (\m -> (:) ("method", jsonString' . T.decodeUtf8 $ renderMethod m)) mbm $
    [ ("handlers", jsonArray $ map methodDocToJSON md) ]

pathDocToJSON :: PathDoc -> JSON
pathDocToJSON PathDoc{..} = jsonObject
    [ ("path", routeToJSON path)
    , ("methods", jsonArray $ map (uncurry methodToJSON) methods)
    ]

groupToJSON :: Maybe T.Text -> [PathDoc] -> JSON
groupToJSON mg ps = jsonObject $
    maybe id (\g -> (:) ("group", jsonString' g)) mg $
    [("paths", jsonArray $ map pathDocToJSON ps)]

documentsToJSON' :: Documents -> JSON
documentsToJSON' Documents{..} = jsonArray $
    groupToJSON Nothing noGroup :
    map (\(g, p) -> groupToJSON (Just g) p) groups

documentsToJSON :: Documents -> L.Text
documentsToJSON = B.toLazyText . unJSON . documentsToJSON'
