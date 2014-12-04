{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Web.Apiary.Session.ClientSession
    ( ClientSessionConfig(..)
    , initClientSession
    , module Web.Apiary.Session
    ) where

import Web.Apiary(MonadIO(..))
import Web.Apiary.Session
import Web.Apiary.Session.Internal
    (Session(Session), SessionBackend(backendMiddleware', genBackendModify))
import Web.Apiary.Cookie(getCookies, deleteCookie, SetCookie(..), setCookie)
import Data.Apiary.Extension(Initializer', initializer')

import Control.Monad.Apiary.Action(insertVault, lookupVault, deleteVault)
import Control.Applicative ((<$))

import Foreign.C.Types(CTime(..))

import System.PosixCompat.Time(epochTime)

import Data.Time(DiffTime, addUTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import qualified Data.ByteString as S
import qualified Data.Serialize as Serialize
import qualified Data.Vault.Lazy as Vault
import Data.Default.Class(Default(def))

import qualified Web.ClientSession as CS

data ClientSessionConfig = ClientSessionConfig
    { csCookieName     :: S.ByteString
    , csCookiePath     :: Maybe S.ByteString
    , csCookieDomain   :: Maybe S.ByteString
    , csCookieHttpOnly :: Bool
    , csCookieSecure   :: Bool
    , csTTL            :: Maybe DiffTime
    , csSessionKey     :: IO CS.Key
    }

data ClientSessionBackend sess (m :: * -> *) = ClientSessionBackend
    { clientSessionEncryptKey :: CS.Key
    , clientSessionVaultKey   :: Vault.Key sess
    , clientSessionConfig     :: ClientSessionConfig
    }

instance Default ClientSessionConfig where
    def = ClientSessionConfig "_sess" (Just "/") Nothing True True
        (Just $ 7 * 24 * 60 * 60) (liftIO CS.getDefaultKey)

initClientSession :: (MonadIO m, Serialize.Serialize sess)
                  => proxy sess -- ^ session type to initialize.
                  -> ClientSessionConfig
                  -> Initializer' m (Session sess m)
initClientSession _ cfg = initializer' $ do
    eKey <- liftIO $ csSessionKey cfg
    vKey <- liftIO Vault.newKey
    return $ Session (ClientSessionBackend eKey vKey cfg)

instance (Serialize.Serialize sess, MonadIO m) => SessionBackend (ClientSessionBackend sess m) sess m where
    backendMiddleware' ClientSessionBackend{clientSessionConfig = ClientSessionConfig{..}, ..} m = do
        cs  <- getCookies
        mbNow <- case lookup csCookieName cs >>= CS.decrypt clientSessionEncryptKey >>=
            either (const Nothing) Just . Serialize.decode of
                Nothing     -> return Nothing
                Just (t, v) -> do
                    case csTTL of
                        Nothing  -> Nothing <$ insertVault clientSessionVaultKey v
                        Just ttl -> do
                            CTime now <- liftIO epochTime
                            if t + round ttl < now
                            then return (Just now)
                            else Just now <$ insertVault clientSessionVaultKey v
        m
        lookupVault clientSessionVaultKey >>= \case
            Nothing -> deleteCookie csCookieName
            Just v  -> do
                now <- maybe (liftIO epochTime >>= \(CTime i) -> return i) return mbNow
                v'  <- liftIO . CS.encryptIO clientSessionEncryptKey $ Serialize.encode (now, v)
                let cCookie = def
                        { setCookieName     = csCookieName
                        , setCookieValue    = v'
                        , setCookiePath     = csCookiePath
                        , setCookieDomain   = csCookieDomain
                        , setCookieHttpOnly = csCookieHttpOnly
                        , setCookieSecure   = csCookieSecure
                        }
                case csTTL of
                    Nothing -> setCookie cCookie
                    Just d  -> do
                        let now' = posixSecondsToUTCTime $ realToFrac now
                        setCookie cCookie
                            { setCookieMaxAge  = Just d
                            , setCookieExpires = Just (addUTCTime (realToFrac d) now')
                            }

    genBackendModify ClientSessionBackend{clientSessionConfig = ClientSessionConfig{..}, ..} f = do
        sess       <- lookupVault clientSessionVaultKey
        (sess', a) <- f sess
        maybe (deleteVault clientSessionVaultKey) (insertVault clientSessionVaultKey) sess'
        return a
