{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}

module Web.Apiary.ClientSession.Internal where

import Language.Haskell.TH

import System.Directory

import "crypto-random" Crypto.Random
import Crypto.Random.AESCtr

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Monad.Apiary.Filter.Internal
import Control.Monad.Apiary.Filter.Internal.Strategy

import Web.Apiary.Wai
import Web.Apiary
import Web.Apiary.Cookie
import Web.ClientSession 
import qualified Network.HTTP.Types as HTTP

import Data.Proxy
import Data.String
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Default.Class
import Data.Binary
import Data.IORef
import Data.Apiary.Document
import Data.Apiary.Extension

import Text.Blaze.Html
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy as L

data Session = Session
    { key           :: Key
    , tokenGen      :: IORef AESRNG
    , sessionConfig :: SessionConfig
    }

data KeySource
    = KeyFile       FilePath
    | KeyByteString S.ByteString

instance IsString KeySource where
    fromString = KeyByteString . fromString

-- | generate and embed key at compile time. since 0.13.2.
--
-- This function embed as SessionsessionConfig with default sessionConfig. so you can sessionConfigure it.
-- but DON'T sessionConfigure sessionKey.
--
-- this function is convenient when create heroku project.
-- 
-- @
-- embedsessionConfig = $embedDefaultKeysessionConfig { csrfTokenCookieName = \"foo\" }
-- @
embedKeyConfig :: FilePath -> ExpQ
embedKeyConfig keyfile = do
    bs <- runIO $ do
        exists <- doesFileExist keyfile
        if exists
            then do 
                b <- S.readFile keyfile
                case initKey b of
                    Left  _ -> newKey
                    Right _ -> return b
        else newKey
    let s = stringE $ SC.unpack bs
    [| def { sessionKey = KeyByteString $s } |]
  where
    newKey = do
        (bs, _) <- randomKey
        S.writeFile keyfile bs
        return bs

embedDefaultKeyConfig :: ExpQ
embedDefaultKeyConfig = embedKeyConfig defaultKeyFile

data SessionConfig = SessionConfig
    { sessionKey            :: KeySource
    , sessionMaxAge         :: DiffTime
    , sessionPath           :: Maybe S.ByteString
    , sessionDomain         :: Maybe S.ByteString
    , sessionHttpOnly       :: Bool
    , sessionSecure         :: Bool

    , sessionTimeoutAction  :: forall e m. MonadIO m => ActionT e m ()

    , angularXsrfCookieName :: Maybe S.ByteString
    , csrfTokenCookieName   :: S.ByteString

    , csrfTokenCheckingName :: Either HTTP.HeaderName S.ByteString
    , csrfTokenLength       :: Int
    }

defaultCheckTokenFailAction :: Monad actM => ActionT exts actM ()
defaultCheckTokenFailAction = do
    reset
    status status401
    bytes "session timeout\n"
    stop

instance Default SessionConfig where
    def = SessionConfig
        (KeyFile defaultKeyFile) (24 * 60 * 60) Nothing Nothing True True
        defaultCheckTokenFailAction Nothing "_token" (Right "_token") 40

makeSession :: MonadIO m => SessionConfig -> m Session
makeSession cfg@SessionConfig{..} = do
    k <- liftIO $ case sessionKey of
        KeyFile       f -> getKey f
        KeyByteString s -> either fail return $ initKey s
    p <- liftIO $ makeSystem >>= newIORef
    let sess = Session k p cfg
    return sess

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

setSession :: MonadIO m => Session -> S.ByteString -> S.ByteString -> ActionT exts m ()
setSession sess k v = do
    s <- liftIO $ mkSessionCookie (sessionConfig sess) (key sess) k v
    setCookie s

newToken :: Int -> IORef AESRNG -> IO S.ByteString
newToken len gen = do
    atomicModifyIORef' gen (\rng -> swap $ withRandomBytes rng len Base64.encode)
  where 
    swap (a,b) = (b,a)

csrfToken :: MonadIO m => Session -> ActionT exts m S.ByteString
csrfToken Session{..} = do
    tok <- liftIO $ newToken (csrfTokenLength sessionConfig) tokenGen 
    sc <- liftIO $ mkSessionCookie sessionConfig key (csrfTokenCookieName sessionConfig) tok
    setCookie sc
    maybe (return ()) (setCookie . ngCookie sc tok) (angularXsrfCookieName sessionConfig)
    return tok
  where
    ngCookie sc tok k = sc { setCookieName     = k
                           , setCookieValue    = tok
                           , setCookieHttpOnly = False
                           }

session :: (MonadIO actM, Strategy w, Query a, Has Session exts)
        => S.ByteString -> w a -> ApiaryT exts (SNext w prms a) actM m () -> ApiaryT exts prms actM m ()
session k p = focus (DocPrecondition $ toHtml (show k) <> " session cookie required") $ \l -> do
    sess <- getExt (Proxy :: Proxy Session)
    r    <- getRequest
    t    <- liftIO getCurrentTime
    let mbr = readStrategy readQuery ((k ==) . fst) p
            (map (second $ getSessionValue sess t) $ cookie' r) l
    maybe mzero return mbr

checkToken :: (MonadIO actM, Has Session exts)
           => ApiaryT exts prms actM m ()
           -> ApiaryT exts prms actM m ()
checkToken = focus (DocPrecondition "CSRF token required") $ \l -> do
    sess@Session{..} <- getExt (Proxy :: Proxy Session)
    r <- getRequest
    p <- getReqParams

    t <- liftIO getCurrentTime
    let stok = getSessionValue sess t =<< 
               lookup (csrfTokenCookieName sessionConfig) (cookie' r)
    guard (isJust stok)
    
    qtok <- return . join $ case csrfTokenCheckingName sessionConfig of
        Right name -> lookup name $ reqParams pByteString r p []
        Left name  -> lookup name $ map (\(k,v) -> (k, Just v)) $ requestHeaders r
    guard (isJust qtok)

    if qtok == stok then return l else sessionTimeoutAction sessionConfig >> mzero
