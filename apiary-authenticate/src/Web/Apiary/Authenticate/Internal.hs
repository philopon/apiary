{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.Authenticate.Internal where

import GHC.Generics(Generic)

import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.Apiary.Filter.Internal

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as Client

import Web.Authenticate.OpenId
import Web.Apiary
import Web.Apiary.ClientSession
import qualified Web.Apiary.Wai as Wai

import Data.Binary
import Data.Data (Data)
import Data.Maybe
import Data.List
import Data.Apiary.SList
import Data.Apiary.Proxy
import Data.Apiary.Extension
import Data.Default.Class

import Blaze.ByteString.Builder
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data AuthConfig = AuthConfig
    { authSessionName   :: S.ByteString
    , authSuccessPage   :: S.ByteString
    , authSessionConfig :: SessionConfig
    , authUrl           :: T.Text

    , authPrefix        :: [T.Text]
    , authReturnToPath  :: [T.Text]
    , authLogoutPath    :: [T.Text]

    , providers         :: [(T.Text, Provider)]
    }

data Provider = Provider
    { providerUrl  :: T.Text
    , realm        :: Maybe T.Text
    , parameters   :: [(T.Text, T.Text)]
    }

instance Default AuthConfig where
    def = AuthConfig "_ID" "/" def "http://localhost:3000" ["auth"] ["return_to"] ["logout"] $ 
        [ ("google", Provider "https://www.google.com/accounts/o8/id" Nothing [])
        , ("yahoo",  Provider "http://me.yahoo.com/"                  Nothing [])
        ]

data Auth = Auth
    { manager           :: Client.Manager
    , config            :: AuthConfig
    }

authWith :: MonadBaseControl IO m
         => Client.Manager
         -> AuthConfig -> (Auth -> m a) -> m a
authWith mgr conf m = m (Auth mgr conf)

authHandler :: (Monad m, MonadIO actM, Has Session exts)
            => Auth -> ApiaryT exts prms actM m ()
authHandler Auth{..} = retH >> mapM_ (uncurry go) (providers config)
  where
    pfxPath p = function id (\_ r -> if p `isPrefixOf` Wai.pathInfo r then Just SNil else Nothing)

    retH = pfxPath (authPrefix config ++ authReturnToPath config) . method GET . action $
        returnAction (authSessionConfig config) manager (authSessionName config) (authSuccessPage config)

    go name Provider{..} = pfxPath (authPrefix config ++ [name]) . method GET . action $
        authAction manager providerUrl returnTo realm parameters

    returnTo = T.decodeUtf8 $ T.encodeUtf8 (authUrl config) `S.append`
        toByteString (HTTP.encodePathSegments (authPrefix config ++ authReturnToPath config))

authorized :: (MonadIO actM, Has Session exts)
           => Auth -> ApiaryT exts (OpenId ': prms) actM m () -> ApiaryT exts prms actM m ()
authorized Auth{..} = session (authSessionName config) (pOne (Proxy :: Proxy OpenId))

authConfig :: Auth -> AuthConfig
authConfig = config

authProviders :: Auth -> [(T.Text, Provider)]
authProviders = providers . config

authRoutes :: Auth -> [(T.Text, S.ByteString)]
authRoutes auth =
    map (\(k,_) -> (k, toByteString . HTTP.encodePathSegments $ authPrefix (config auth) ++ [k])) $
    providers (config auth)

authLogout :: Monad m => Auth -> ActionT exts m ()
authLogout auth = deleteCookie (authSessionName $ config auth)

authAction :: MonadIO m => Client.Manager -> T.Text -> T.Text
           -> Maybe T.Text -> [(T.Text, T.Text)] -> ActionT exts m ()
authAction mgr uri returnTo realm param = do
    fw <- liftIO . runResourceT $ getForwardUrl uri returnTo realm param mgr
    redirect $ T.encodeUtf8 fw

data OpenId_ a = OpenId_
    { opLocal :: a
    , params  :: [(a, a)]
    , claimed :: Maybe a
    } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)
instance Binary (OpenId_ S.ByteString)

instance Binary (OpenId_ T.Text) where
    get   = fmap (fmap T.decodeUtf8) (get :: Get (OpenId_ S.ByteString))
    put g = put (fmap T.encodeUtf8 g)

instance Query (OpenId_ T.Text) where
    readQuery Nothing  = Nothing
    readQuery (Just s) = case decodeOrFail (L.fromStrict s) of
        Right (s',_,a) | L.null s' -> Just a
        _                          -> Nothing
    qTypeRep = typeRep

type OpenId = OpenId_ T.Text

toOpenId :: OpenIdResponse -> OpenId
toOpenId r = OpenId_ 
    (identifier $ oirOpLocal r)
    (oirParams r)
    (identifier <$> oirClaimed r)

returnAction :: (MonadIO m, Has Session exts)
             => SessionConfig -> Client.Manager
             -> S.ByteString -> S.ByteString -> ActionT exts m ()
returnAction sc mgr key to = do
    q <- Wai.queryString <$> getRequest
    r <- liftIO . runResourceT $ authenticateClaimed (mapMaybe queryElem q) mgr
    setSessionWith sc key . L.toStrict $ encode (toOpenId r)
    redirect to
  where
    queryElem (_, Nothing) = Nothing
    queryElem (k, Just v)  = Just (T.decodeUtf8 k, T.decodeUtf8 v)
