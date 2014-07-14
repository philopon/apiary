{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}

module Web.Apiary.ClientSession.Internal where

import Control.Monad
import Control.Applicative
import Control.Arrow

import Web.Apiary.Wai
import qualified Network.HTTP.Types as HTTP

import Web.Apiary hiding (Default(..))
import Web.Apiary.Cookie
import Web.Apiary.Cookie.Internal
import Web.ClientSession 
import Data.Maybe
import Data.Time
import Data.Default.Class
import Data.Binary
import Data.IORef
import qualified Data.ByteString.Base64 as Base64

import "crypto-random" Crypto.Random
import Crypto.Random.AESCtr

import Control.Monad.Apiary.Filter.Internal
import Control.Monad.Apiary.Filter.Internal.Strategy

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


data Session = Session
    { key       :: Key
    , tokenGen  :: IORef AESRNG
    , config    :: SessionConfig
    }

data SessionConfig = SessionConfig
    { sessionKeyFile        :: FilePath
    , sessionMaxAge         :: DiffTime
    , sessionPath           :: Maybe S.ByteString
    , sessionDomain         :: Maybe S.ByteString
    , sessionHttpOnly       :: Bool
    , sessionSecure         :: Bool

    , angularXsrfCookieName :: Maybe S.ByteString
    , csrfTokenCookieName   :: S.ByteString

    , csrfTokenCheckingName :: Either HTTP.HeaderName S.ByteString
    , csrfTokenLength       :: Int
    }

instance Default SessionConfig where
    def = SessionConfig
        defaultKeyFile (24 * 60 * 60) Nothing Nothing True True
        Nothing "_token" (Right "_token") 40

withSession :: MonadIO m => SessionConfig -> (Session -> m b) -> m b
withSession cfg@SessionConfig{..} m = do
    k <- liftIO $ getKey sessionKeyFile
    p <- liftIO $ makeSystem >>= newIORef
    let sess = Session k p cfg
    m sess

newtype BinUTCTime = BinUTCTime { getUTCTime :: UTCTime }

instance Binary BinUTCTime where
    put (BinUTCTime t) = do
        put . toModifiedJulianDay $ utctDay t
        put . toRational $ utctDayTime t
    get = do
        d <- ModifiedJulianDay <$> get
        t <- fromRational      <$> get
        return . BinUTCTime $ UTCTime d t

mkSessionCookie :: SessionConfig -> Key -> S.ByteString -> S.ByteString -> IO SetCookie
mkSessionCookie conf key k v = do
    t <- getCurrentTime
    let expire = addUTCTime (realToFrac $ sessionMaxAge conf) t
    v' <- encryptIO key $ L.toStrict $ encode (BinUTCTime expire, v)
    return def { setCookieName     = k
               , setCookieValue    = v'
               , setCookiePath     = sessionPath conf
               , setCookieExpires  = Just expire
               , setCookieMaxAge   = Just (sessionMaxAge conf)
               , setCookieDomain   = sessionDomain conf
               , setCookieHttpOnly = sessionHttpOnly conf
               , setCookieSecure   = sessionSecure conf
               }

getSessionValue :: Session -> UTCTime -- ^ current time
                -> S.ByteString 
                -> Maybe S.ByteString
getSessionValue Session{key = k} c s = decrypt k s >>= \s' -> case decodeOrFail (L.fromStrict s') of
        Right (_, _, (BinUTCTime t, v)) -> if c < t then Just v else Nothing
        _ -> Nothing

setSession :: MonadIO m => Session -> S.ByteString -> S.ByteString -> ActionT m ()
setSession sess k v = do
    s <- liftIO $ mkSessionCookie (config sess) (key sess) k v
    setCookie s

newToken :: Int -> IORef AESRNG -> IO S.ByteString
newToken len gen = do
    atomicModifyIORef' gen (\rng -> swap $ withRandomBytes rng len Base64.encode)
  where 
    swap (a,b) = (b,a)

csrfToken :: MonadIO m => Session -> ActionT m S.ByteString
csrfToken Session{..} = do
    tok <- liftIO $ newToken (csrfTokenLength config) tokenGen 
    sc <- liftIO $ mkSessionCookie config key (csrfTokenCookieName config) tok
    setCookie sc
    maybe (return ()) (setCookie . ngCookie sc tok) (angularXsrfCookieName config)
    return tok
  where
    ngCookie sc tok k = sc { setCookieName     = k
                           , setCookieValue    = tok
                           , setCookieHttpOnly = False
                           }

session :: (Functor n, MonadIO n, Strategy w, Query a) => Session
        -> S.ByteString -> proxy (w a) -> ApiaryT (SNext w as a) n m b -> ApiaryT as n m b
session sess k p = focus id $ \l -> do
    r   <- getRequest
    t   <- liftIO getCurrentTime
    let mbr = readStrategy readQuery ((k ==) . fst) p
            (map (second $ getSessionValue sess t) $ cookie' r) l
    maybe empty return mbr

checkToken :: (Functor n, MonadIO n)
           => Session
           -> ApiaryT c n m a
           -> ApiaryT c n m a
checkToken sess@Session{..} = focus id $ \l -> do
    r <- getRequest
    p <- getReqParams

    t <- liftIO getCurrentTime
    let stok = getSessionValue sess t =<< 
               lookup (csrfTokenCookieName config) (cookie' r)
    guard (isJust stok)
    
    qtok <- return . join $ case csrfTokenCheckingName config of
        Right name -> lookup name $ reqParams pByteString r p []
        Left name  -> lookup name $ map (\(k,v) -> (k, Just v)) $ requestHeaders r
    guard (isJust qtok)

    if qtok == stok then return l else empty

