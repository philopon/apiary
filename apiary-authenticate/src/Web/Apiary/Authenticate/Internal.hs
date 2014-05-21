{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}

module Web.Apiary.Authenticate.Internal where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Applicative

import Data.Maybe
import Data.List
import Data.Default.Class
import qualified Data.ByteString.Char8 as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Blaze.ByteString.Builder

import qualified Network.Wai as Wai
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

type HasAuth = (?webApiaryAuthenticateAuth :: Auth, HasSession)

withAuth :: HasSession => AuthConfig -> (HasAuth => Apiary c ()) -> Apiary c ()
withAuth = withAuthWith tlsManagerSettings

withAuthWith :: HasSession => Client.ManagerSettings -> AuthConfig
             -> (HasAuth => Apiary c ()) -> Apiary c ()
withAuthWith s conf m = withManager s $ \mgr -> do
    let ?webApiaryAuthenticateAuth = Auth mgr conf
    addAuthHandler (Auth mgr conf) m


withManager :: MonadBaseControl IO m => Client.ManagerSettings -> (Client.Manager -> m a) -> m a
withManager conf f = control $ \run -> Client.withManager conf (\mgr -> run $ f mgr)

addAuthHandler :: HasSession => Auth -> Apiary c () -> Apiary c ()
addAuthHandler Auth{..} m = m >> retH >> mapM_ (uncurry go) (providers config)
  where
    pfxPath p = function (\_ r -> if p `isPrefixOf` Wai.pathInfo r then Just SNil else Nothing)

    retH = pfxPath (authPrefix config ++ authReturnToPath config) . stdMethod GET . action $
        returnAction manager (authSessionName config) (authSuccessPage config)

    go name Provider{..} = pfxPath (authPrefix config ++ [name]) . stdMethod GET . action $
        authAction manager providerUrl returnTo realm parameters

    returnTo = T.decodeUtf8 $ T.encodeUtf8 (authUrl config) `S.append`
        toByteString (HTTP.encodePathSegments (authPrefix config ++ authReturnToPath config))


authorized :: HasAuth => Apiary (Snoc as S.ByteString) a -> Apiary as a
authorized = session (authSessionName $ config ?webApiaryAuthenticateAuth) (pOne pByteString)

authConfig :: HasAuth => Action AuthConfig
authConfig = return (config ?webApiaryAuthenticateAuth)

authProviders :: HasAuth => Action [(T.Text, Provider)]
authProviders = providers <$> authConfig

authRoutes :: HasAuth => Action [(T.Text, S.ByteString)]
authRoutes = do 
    conf <- authConfig
    return . map (\(k,_) -> (k, toByteString . HTTP.encodePathSegments $ authPrefix conf ++ [k])) $ providers conf

authLogout :: HasAuth => Action ()
authLogout = do
    conf <- authConfig
    deleteCookie (authSessionName conf)

authAction :: Client.Manager -> T.Text -> T.Text
           -> Maybe T.Text -> [(T.Text, T.Text)] -> Action ()
authAction mgr uri returnTo realm param = do
    fw <- liftIO . runResourceT $ getForwardUrl uri returnTo realm param mgr
    redirect $ T.encodeUtf8 fw

returnAction :: HasSession => Client.Manager -> S.ByteString -> S.ByteString -> Action ()
returnAction mgr key to = do
    q <- Wai.queryString <$> getRequest
    r <- liftIO . runResourceT $ authenticateClaimed (mapMaybe queryElem q) mgr
    setSession key (T.encodeUtf8 . identifier $ oirOpLocal r)
    redirect to
  where
    queryElem (_, Nothing) = Nothing
    queryElem (k, Just v)  = Just (T.decodeUtf8 k, T.decodeUtf8 v)
