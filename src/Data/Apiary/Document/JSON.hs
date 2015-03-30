{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Apiary.Document.JSON
    ( JSON(unJSON)
    , string', string
    , object, array
    , documentsToJSON
    ) where

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

string' :: T.Text -> JSON
string' s = JSON $ B.singleton '"' <> B.fromText (escapeJSON' s) <> B.singleton '"'

string :: L.Text -> JSON
string s = JSON $ B.singleton '"' <> B.fromLazyText (escapeJSON s) <> B.singleton '"'

object :: [(T.Text, JSON)] -> JSON
object = JSON . obj . mconcat . intersperse (B.singleton ',') .
    map (\(k,JSON v) -> unJSON (string' k) <> B.singleton ':' <> v)
  where
    obj b = B.singleton '{' <> b <> B.singleton '}'

array :: [JSON] -> JSON
array = JSON . ary . mconcat . intersperse (B.singleton ',') . unJSONList
  where
    ary b = B.singleton '[' <> b <> B.singleton ']'

routeToJSON' :: Route -> [JSON]
routeToJSON' (Path p n) = object obj : routeToJSON' n
  where
    obj = [ ("tag",  string' "path")
          , ("path", string' p)
          ]
routeToJSON' (Fetch k t md n) = object obj : routeToJSON' n
  where
    obj = (:) ("tag",  string' "fetch") $
          (:) ("name", string' k) $
          (:) ("type", string' . T.pack $ show t) $
          maybe id (\d -> (:) ("doc", string $ toLazyText d)) md $
          []
routeToJSON' (Rest k md) = (:[]) $ object $
    (:) ("tag", string' "rest") $
    (:) ("name", string' k) $
    maybe id (\d -> (:) ("doc", string $ toLazyText d)) md $
    []
routeToJSON' Any = [object [ ("tag", string' "rest") ]]
routeToJSON' End = []

routeToJSON :: Route -> JSON
routeToJSON = array . routeToJSON'

queryDocToJSON :: QueryDoc -> JSON
queryDocToJSON QueryDoc{..} = object $
    (:) ("name", string' queryName) $
    (:) ("strategy", string' $ strategyInfo queryStrategy) $
    (:) ("query", string' query) $
    maybe id (\t -> (:) ("type", string' . T.pack $ show t)) queryType $
    maybe id (\d -> (:) ("doc", string $ toLazyText d)) queryDocument $
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
methodDocToJSON MethodDoc{..} = object $
    (:) ("queries", array $ map queryDocToJSON queries) $
    (:) ("preconditions", array $ map (string . toLazyText) preconditions) $
    maybe id (\a -> (:) ("accept", string' $ T.decodeUtf8 a)) accept $
    [("doc", string $ toLazyText document)]

methodToJSON :: Maybe Method -> [MethodDoc] -> JSON
methodToJSON mbm md = object $
    maybe id (\m -> (:) ("method", string' . T.decodeUtf8 $ renderMethod m)) mbm $
    [ ("handlers", array $ map methodDocToJSON md) ]

pathDocToJSON :: PathDoc -> JSON
pathDocToJSON PathDoc{..} = object
    [ ("path", routeToJSON path)
    , ("methods", array $ map (uncurry methodToJSON) methods)
    ]

groupToJSON :: Maybe T.Text -> [PathDoc] -> JSON
groupToJSON mg ps = object $
    maybe id (\g -> (:) ("group", string' g)) mg $
    [("paths", array $ map pathDocToJSON ps)]

documentsToJSON :: Documents -> JSON
documentsToJSON Documents{..} = array $
    groupToJSON Nothing noGroup :
    map (\(g, p) -> groupToJSON (Just g) p) groups
