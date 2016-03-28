{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Data.Apiary.Param
    ( -- * route path parameter
      Path(..)
    , readPathAs
      -- * query parameter
    , Query(..)
    , QueryRep(..)
    , File(..)
    -- * request parameter
    , Param
    , ReqParam(..)
    -- * Strategy
    , Strategy(..)
    , StrategyRep(..)
    , First(..)
    , One(..)
    , Many(..)
    , Some(..)
    , Option(..)
    , Optional(..)
      -- * Proxies
    , pBool
    , pInt
    , pWord
    , pDouble
    , pText
    , pLazyText
    , pByteString
    , pLazyByteString
    , pString
    , pMaybe
    , pFile
    -- ** strategy
    , pFirst
    , pOne
    , pMany
    , pSome
    , pOption
    , pOptional
    ) where

import Control.Monad(when, unless, MonadPlus(mzero))
import Control.Arrow(second)

import qualified Network.HTTP.Types as HTTP

import Data.Int(Int8, Int16, Int32, Int64)
#if MIN_VERSION_base(4,8,0)
import Data.Word(Word8, Word16, Word32, Word64)
#else
import Data.Word(Word, Word8, Word16, Word32, Word64)
#endif
import Data.Maybe(isJust, catMaybes)
import Network.Routing.Dict(KV((:=)), type (</), Store)
import qualified Network.Routing.Dict as Dict
import Data.Typeable.Compat
    ( Typeable, mkTyConApp, typeRepTyCon, typeOf, TypeRep, typeRep, Proxy(..))

import GHC.TypeLits.Compat(Symbol, KnownSymbol)

import Data.String(IsString)
import Data.Time.Calendar(Day, fromGregorian)
import qualified Data.Text.Read as TR
import           Data.Text.Encoding.Error(lenientDecode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Data.ByteString.Read as SL

jsToBool :: (IsString a, Eq a) => a -> Bool
jsToBool = flip notElem jsFalse
  where
    jsFalse = ["false", "0", "-0", "", "null", "undefined", "NaN"]

-- | readPath providing type using Proxy.
readPathAs :: Path a => proxy a -> T.Text -> Maybe a
readPathAs _ t = readPath t
{-# INLINE readPathAs #-}

data Text deriving Typeable

type Param = (S.ByteString, S.ByteString)

data File
    = File {
          fileParameter   :: S.ByteString
        , fileName        :: S.ByteString
        , fileContentType :: S.ByteString
        , fileContent     :: Either L.ByteString FilePath
        }
    deriving (Show, Eq, Typeable)

data QueryRep
    = Strict   TypeRep -- ^ require value
    | Nullable TypeRep -- ^ allow key only value
    | Check            -- ^ check existance
    | NoValue
    deriving (Show, Eq)

class Path a where
    -- | read route path parameter.
    readPath :: T.Text
             -> Maybe a -- ^ Nothing is failed.
    -- | pretty type of route path parameter.
    pathRep  :: proxy a -> TypeRep

instance Path Char where
    pathRep = typeRep
    readPath s
        | T.null s  = Nothing
        | otherwise = Just $ T.head s

readText :: TR.Reader a -> T.Text -> (Maybe a)
readText p s = case p s of
    Right (a, t) -> if T.null t then Just a else Nothing
    Left   _     -> Nothing

readTextInt :: Integral i => T.Text -> Maybe i
readTextInt = readText (TR.signed TR.decimal)

readTextWord :: Integral i => T.Text -> Maybe i
readTextWord = readText TR.decimal

readTextDouble :: T.Text -> Maybe Double
readTextDouble = readText TR.double

-- | javascript boolean.
-- when \"false\", \"0\", \"-0\", \"\", \"null\", \"undefined\", \"NaN\" then False, else True. since 0.6.0.0.
instance Path Bool    where readPath = Just . jsToBool; pathRep = typeRep

instance Path Int     where readPath = readTextInt; pathRep = typeRep
instance Path Int8    where readPath = readTextInt; pathRep = typeRep
instance Path Int16   where readPath = readTextInt; pathRep = typeRep
instance Path Int32   where readPath = readTextInt; pathRep = typeRep
instance Path Int64   where readPath = readTextInt; pathRep = typeRep
instance Path Integer where readPath = readTextInt; pathRep = typeRep

instance Path Word    where readPath = readTextWord; pathRep = typeRep
instance Path Word8   where readPath = readTextWord; pathRep = typeRep
instance Path Word16  where readPath = readTextWord; pathRep = typeRep
instance Path Word32  where readPath = readTextWord; pathRep = typeRep
instance Path Word64  where readPath = readTextWord; pathRep = typeRep

instance Path Double  where readPath = readTextDouble; pathRep = typeRep
instance Path Float   where readPath = fmap realToFrac . readTextDouble; pathRep = typeRep

instance Path  T.Text      where readPath = Just;                 pathRep _ = typeRep (Proxy :: Proxy Text)
instance Path TL.Text      where readPath = Just . TL.fromStrict; pathRep _ = typeRep (Proxy :: Proxy Text)
instance Path S.ByteString where readPath = Just . T.encodeUtf8;  pathRep _ = typeRep (Proxy :: Proxy Text)
instance Path L.ByteString where readPath = Just . TL.encodeUtf8 . TL.fromStrict; pathRep _ = typeRep (Proxy :: Proxy Text)
instance Path String       where readPath = Just . T.unpack;      pathRep _ = typeRep (Proxy :: Proxy Text)


--------------------------------------------------------------------------------

class Query a where
    -- | read query parameter.
    readQuery :: Maybe S.ByteString -- ^ value of query parameter. Nothing is key only parameter.
              -> Maybe a -- ^ Noting is fail.

    -- | pretty query parameter.
    queryRep  :: proxy a            -> QueryRep
    queryRep = Strict . qTypeRep
    qTypeRep  :: proxy a            -> TypeRep

readBS :: (S.ByteString -> Maybe (a, S.ByteString))
       -> S.ByteString -> Maybe a
readBS p b = case p b of
    Just (i, s) -> if S.null s then Just i else Nothing
    _           -> Nothing

readBSInt :: Integral a => S.ByteString -> Maybe a
readBSInt = readBS (SL.signed SL.integral)

readBSWord :: Integral a => S.ByteString -> Maybe a
readBSWord = readBS SL.integral

readBSDouble :: S.ByteString -> Maybe Double
readBSDouble = readBS (SL.signed SL.double)

-- | javascript boolean.
-- when \"false\", \"0\", \"-0\", \"\", \"null\", \"undefined\", \"NaN\" then False, else True. since 0.6.0.0.
instance Query Bool    where
    readQuery (Just b) = Just $ jsToBool b
    readQuery Nothing  = Just True
    qTypeRep = typeRep

instance Query Int     where readQuery = maybe Nothing readBSInt; qTypeRep = typeRep
instance Query Int8    where readQuery = maybe Nothing readBSInt; qTypeRep = typeRep
instance Query Int16   where readQuery = maybe Nothing readBSInt; qTypeRep = typeRep
instance Query Int32   where readQuery = maybe Nothing readBSInt; qTypeRep = typeRep
instance Query Int64   where readQuery = maybe Nothing readBSInt; qTypeRep = typeRep
instance Query Integer where readQuery = maybe Nothing readBSInt; qTypeRep = typeRep

instance Query Word    where readQuery = maybe Nothing readBSWord; qTypeRep = typeRep
instance Query Word8   where readQuery = maybe Nothing readBSWord; qTypeRep = typeRep
instance Query Word16  where readQuery = maybe Nothing readBSWord; qTypeRep = typeRep
instance Query Word32  where readQuery = maybe Nothing readBSWord; qTypeRep = typeRep
instance Query Word64  where readQuery = maybe Nothing readBSWord; qTypeRep = typeRep

instance Query Double  where readQuery = maybe Nothing readBSDouble; qTypeRep = typeRep
instance Query Float   where readQuery = maybe Nothing (fmap realToFrac . readBSDouble); qTypeRep = typeRep

instance Query T.Text where
    readQuery  = fmap $ T.decodeUtf8With lenientDecode
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

instance Query TL.Text where
    readQuery  = fmap (TL.decodeUtf8With lenientDecode . L.fromStrict)
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

instance Query S.ByteString where
    readQuery  = id
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

instance Query L.ByteString where
    readQuery  = fmap L.fromStrict
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

instance Query String where
    readQuery  = fmap S.unpack
    qTypeRep _ = typeRep (Proxy :: Proxy Text)

-- | fuzzy date parse. three decimal split by 1 char.
-- if year < 100 then + 2000. since 0.16.0.
--
-- example:
--
-- * 2014-02-05
-- * 14-2-5
-- * 14.2.05
instance Query Day where
    readQuery = (>>= \s0 -> do
        (y, s1) <- SL.integral s0
        when (S.null s1) Nothing
        (m, s2) <- SL.integral $ S.tail s1
        when (S.null s2) Nothing
        (d, s3) <- SL.integral $ S.tail s2
        unless (S.null s3) Nothing
        let y' = if y < 100 then 2000 + y else y
        return $ fromGregorian y' m d)
    qTypeRep _ = typeRep (Proxy :: Proxy Day)

-- | fuzzy date parse. three decimal split by 1 char.
-- if year < 100 then + 2000. since 0.16.0.
--
-- example:
--
-- * 2014-02-05
-- * 14-2-5
-- * 14.2.05
instance Path Day where
    readPath s0 = either (const Nothing) Just $ do
        (y, s1) <- TR.decimal s0
        when (T.null s1) (Left "")
        (m, s2) <- TR.decimal (T.tail s1)
        when (T.null s2) (Left "")
        (d, s3) <- TR.decimal (T.tail s2)
        unless (T.null s3) (Left "")
        let y' = if y < 100 then 2000 + y else y
        return $ fromGregorian y' m d
    pathRep _ = typeRep (Proxy :: Proxy Day)

-- | allow no parameter. but check parameter type.
instance Query a => Query (Maybe a) where
    readQuery (Just a) = Just `fmap` readQuery (Just a)
    readQuery Nothing  = Just Nothing
    queryRep _         = Nullable $ qTypeRep (Proxy :: Proxy a)
    qTypeRep _         = maybeCon `mkTyConApp` [qTypeRep (Proxy :: Proxy a)]
      where maybeCon = typeRepTyCon $ typeOf (Nothing :: Maybe ())

-- | always success. for check existence.
instance Query () where
    readQuery _ = Just ()
    queryRep  _ = Check
    qTypeRep  _ = typeOf ()

pBool :: Proxy Bool
pBool = Proxy

pInt :: Proxy Int
pInt = Proxy

pWord :: Proxy Word
pWord = Proxy

pDouble :: Proxy Double
pDouble = Proxy

pText :: Proxy T.Text
pText = Proxy
pLazyText :: Proxy TL.Text
pLazyText = Proxy
pByteString :: Proxy S.ByteString
pByteString = Proxy
pLazyByteString :: Proxy L.ByteString
pLazyByteString = Proxy
pString :: Proxy String
pString = Proxy

pMaybe :: proxy a -> Proxy (Maybe a)
pMaybe _ = Proxy

pFile :: Proxy File
pFile = Proxy

class ReqParam a where
    reqParams   :: proxy a -> HTTP.Query -> [Param] -> [File] -> [(S.ByteString, Maybe a)]
    reqParamRep :: proxy a -> QueryRep

instance ReqParam File where
    reqParams _ _ _ = map (\f -> (fileParameter f, Just f))
    reqParamRep   _ = Strict $ typeRep pFile

instance Query a => ReqParam a where
    reqParams _ q p _ = map (second readQuery) q ++
        map (second $ readQuery . Just) p
    reqParamRep = queryRep

newtype StrategyRep = StrategyRep
    { strategyInfo :: T.Text }
    deriving (Show, Eq)


class Strategy (w :: * -> *) where
    type SNext w (k::Symbol) a (prms :: [KV *]) :: [KV *]
    strategy :: (KnownSymbol k, k </ prms, MonadPlus m) => w a -> proxy' k -> [Maybe a] -> Store prms -> m (Store (SNext w k a prms))
    strategyRep :: w a -> StrategyRep

data First a = First
instance Strategy First where
    type SNext First k a ps = k ':= a ': ps
    strategy _ k (Just a:_) d = return $ Dict.add k a d
    strategy _ _ _          _ = mzero
    strategyRep _ = StrategyRep "first"

data One a = One
instance Strategy One where
    type SNext One k a ps = k ':= a ': ps
    strategy _ k [Just a] d = return $ Dict.add k a d
    strategy _ _ _        _ = mzero
    strategyRep _ = StrategyRep "one"

data Many a = Many
instance Strategy Many where
    type SNext Many k a ps = k ':= [a] ': ps
    strategy _ k as d = if all isJust as then return $ Dict.add k (catMaybes as) d else mzero
    strategyRep _ = StrategyRep "many"

data Some a = Some
instance Strategy Some where
    type SNext Some k a ps = k ':= [a] ': ps
    strategy _ _ [] _ = mzero
    strategy _ k as d = if all isJust as then return $ Dict.add k (catMaybes as) d else mzero
    strategyRep _ = StrategyRep "some"

data Option a = Option
instance Strategy Option where
    type SNext Option k a ps = k ':= Maybe a ': ps
    strategy _ k (Just a:_)  d = return $ Dict.add k (Just a) d
    strategy _ _ (Nothing:_) _ = mzero
    strategy _ k []          d = return $ Dict.add k Nothing d
    strategyRep _ = StrategyRep "option"

data Optional a = Optional T.Text a
instance Strategy Optional where
    type SNext Optional k a ps = k ':= a ': ps
    strategy _              k (Just a:_)  d = return $ Dict.add k a d
    strategy _              _ (Nothing:_) _ = mzero
    strategy (Optional _ a) k []          d = return $ Dict.add k a d
    strategyRep (Optional a _) = StrategyRep $ "default:" `T.append` a

pFirst :: proxy a -> First a
pFirst _ = First

pOne :: proxy a -> One a
pOne _ = One

pMany :: proxy a -> Many a
pMany _ = Many

pSome :: proxy a -> Some a
pSome _ = Some

pOption :: proxy a -> Option a
pOption _ = Option

pOptional :: Show a => a -> Optional a
pOptional a = Optional (T.pack $ show a) a
