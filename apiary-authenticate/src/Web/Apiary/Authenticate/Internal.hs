{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Web.Apiary.Authenticate.Internal where

import Control.Monad.Trans.Resource
import Control.Applicative

import GHC.Generics(Generic)
import Data.Binary
import Data.Data
import Data.Maybe
import Data.List
import Data.Default.Class
#if __GLASGOW_HASKELL__ < 707
import Data.Proxy -- for ghc-7.6
#endif
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Blaze.ByteString.Builder

import qualified Web.Apiary.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Web.Authenticate.OpenId

import Web.Apiary hiding(Default(..))
import Data.Apiary.SList
import Control.Monad.Apiary.Filter.Internal
import Web.Apiary.ClientSession.Explicit

data AuthConfig = AuthConfig
    { authSessionName  :: S.ByteString
    , authSuccessPage  :: S.ByteString
    , authUrl          :: T.Text

    , authPrefix       :: [T.Text]
    , authReturnToPath :: [T.Text]
    , authLogoutPath   :: [T.Text]

    , providers        :: [(T.Text, Provider)]
    }

data Provider = Provider
    { providerUrl  :: T.Text
    , realm        :: Maybe T.Text
    , parameters   :: [(T.Text, T.Text)]
    }

instance Default AuthConfig where
    def = AuthConfig "_ID" "/" "http://localhost:3000" ["auth"] ["return_to"] ["logout"] $ 
        [ ("google", Provider "https://www.google.com/accounts/o8/id" Nothing [])
        , ("yahoo",  Provider "http://me.yahoo.com/"                  Nothing [])
        ]

data Auth = Auth
    { manager     :: Client.Manager
    , config      :: AuthConfig
    , authSession :: Session
    }

withAuth :: Session -> AuthConfig -> (Auth -> IO a) -> IO a
withAuth sess = withAuthWith sess tlsManagerSettings

withAuthWith :: Session -> Client.ManagerSettings
             -> AuthConfig -> (Auth -> IO a) -> IO a
withAuthWith sess s conf m = Client.withManager s $ \mgr -> 
    m (Auth mgr conf sess)

authHandler :: (Functor n, MonadIO n) => Auth -> ApiaryT c n m ()
authHandler Auth{..} = retH >> mapM_ (uncurry go) (providers config)
  where
    pfxPath p = function id (\_ r -> if p `isPrefixOf` Wai.pathInfo r then Just SNil else Nothing)

    retH = pfxPath (authPrefix config ++ authReturnToPath config) . stdMethod GET . action $
        returnAction authSession manager (authSessionName config) (authSuccessPage config)

    go name Provider{..} = pfxPath (authPrefix config ++ [name]) . stdMethod GET . action $
        authAction manager providerUrl returnTo realm parameters

    returnTo = T.decodeUtf8 $ T.encodeUtf8 (authUrl config) `S.append`
        toByteString (HTTP.encodePathSegments (authPrefix config ++ authReturnToPath config))

authorized :: Auth -> Apiary (Snoc as OpenId) a -> Apiary as a
authorized Auth{..} = session authSession (authSessionName config) (pOne (Proxy :: Proxy OpenId))

authConfig :: Auth -> AuthConfig
authConfig = config

authProviders :: Auth -> [(T.Text, Provider)]
authProviders = providers . config

authRoutes :: Auth -> [(T.Text, S.ByteString)]
authRoutes auth =
    map (\(k,_) -> (k, toByteString . HTTP.encodePathSegments $ authPrefix (config auth) ++ [k])) $
    providers (config auth)

authLogout :: Monad m => Auth -> ActionT m ()
authLogout auth = deleteCookie (authSessionName $ config auth)

authAction :: MonadIO m => Client.Manager -> T.Text -> T.Text
           -> Maybe T.Text -> [(T.Text, T.Text)] -> ActionT m ()
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

type OpenId = OpenId_ T.Text

toOpenId :: OpenIdResponse -> OpenId
toOpenId r = OpenId_ 
    (identifier $ oirOpLocal r)
    (oirParams r)
    (identifier <$> oirClaimed r)

returnAction :: MonadIO m => Session -> Client.Manager -> S.ByteString -> S.ByteString -> ActionT m ()
returnAction sess mgr key to = do
    q <- Wai.queryString <$> getRequest
    r <- liftIO . runResourceT $ authenticateClaimed (mapMaybe queryElem q) mgr
    setSession sess key . L.toStrict $ encode (toOpenId r)
    redirect to
  where
    queryElem (_, Nothing) = Nothing
    queryElem (k, Just v)  = Just (T.decodeUtf8 k, T.decodeUtf8 v)
