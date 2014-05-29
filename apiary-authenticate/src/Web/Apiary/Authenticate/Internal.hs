{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Web.Apiary.Authenticate.Internal where

import Control.Monad.Trans.Resource
import Control.Applicative

import GHC.Generics(Generic)
import Data.Binary
import Data.Data
import Data.Maybe
import Data.List
import Data.Proxy
import Data.Default.Class
import Data.Reflection
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
import Web.Apiary.ClientSession

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
    { manager :: Client.Manager
    , config  :: AuthConfig
    }

type HasAuth = (Given Auth, HasSession)

withAuth :: HasSession => AuthConfig -> (HasAuth => IO a) -> IO a
withAuth = withAuthWith tlsManagerSettings

withAuthWith :: HasSession => Client.ManagerSettings
             -> AuthConfig -> (HasAuth => IO a) -> IO a
withAuthWith s conf m = Client.withManager s $ \mgr -> 
    give (Auth mgr conf) m

-- | default auth handlers. since 0.8.0.0.
authHandler :: (Functor n, MonadIO n, HasAuth) => ApiaryT c n m ()
authHandler = retH >> mapM_ (uncurry go) (providers config)
  where
    Auth{..}  = given
    pfxPath p = function (\_ r -> if p `isPrefixOf` Wai.pathInfo r then Just SNil else Nothing)

    retH = pfxPath (authPrefix config ++ authReturnToPath config) . stdMethod GET . action $
        returnAction manager (authSessionName config) (authSuccessPage config)

    go name Provider{..} = pfxPath (authPrefix config ++ [name]) . stdMethod GET . action $
        authAction manager providerUrl returnTo realm parameters

    returnTo = T.decodeUtf8 $ T.encodeUtf8 (authUrl config) `S.append`
        toByteString (HTTP.encodePathSegments (authPrefix config ++ authReturnToPath config))


-- | filter which check whether logged in or not, and get id. since 0.7.0.0.
authorized :: HasAuth => Apiary (Snoc as OpenId) a -> Apiary as a
authorized = session (authSessionName $ config given) (pOne (Proxy :: Proxy OpenId))

-- | get auth config. since 0.7.0.0.
authConfig :: (Monad m, HasAuth) => ActionT m AuthConfig
authConfig = return (config given)

-- | get providers. since 0.7.0.0.
authProviders :: (Monad m, HasAuth) => ActionT m [(T.Text, Provider)]
authProviders = providers <$> authConfig

-- | get authenticate routes: (title, route). since 0.7.0.0.
authRoutes :: (Monad m, HasAuth) => ActionT m [(T.Text, S.ByteString)]
authRoutes = do 
    conf <- authConfig
    return . map (\(k,_) -> (k, toByteString . HTTP.encodePathSegments $ authPrefix conf ++ [k])) $ providers conf

-- | delete session. since 0.7.0.0.
authLogout :: (Monad m,  HasAuth) => ActionT m ()
authLogout = do
    conf <- authConfig
    deleteCookie (authSessionName conf)

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

returnAction :: (MonadIO m, HasSession) => Client.Manager -> S.ByteString -> S.ByteString -> ActionT m ()
returnAction mgr key to = do
    q <- Wai.queryString <$> getRequest
    r <- liftIO . runResourceT $ authenticateClaimed (mapMaybe queryElem q) mgr
    setSession key . L.toStrict $ encode (toOpenId r)
    redirect to
  where
    queryElem (_, Nothing) = Nothing
    queryElem (k, Just v)  = Just (T.decodeUtf8 k, T.decodeUtf8 v)
