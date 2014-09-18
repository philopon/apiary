{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.Heroku
    ( Heroku, HerokuConfig(..)
    , heroku, herokuWith
    , getHerokuEnv, getHerokuEnv'
    ) where

import System.Environment
import System.Process
import System.Exit

import Control.Exception
import Control.Arrow hiding (app)
import Control.Applicative
import Control.Monad.Trans

import Data.IORef
import Data.Proxy
import Data.Default.Class
import qualified Data.HashMap.Strict as H
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Network.Wai
import Control.Monad.Apiary
import Data.Apiary.Extension
import Data.Apiary.Extension.Internal

data Heroku = Heroku 
    { herokuEnv    :: IORef (Maybe (H.HashMap T.Text T.Text))
    , herokuConfig :: HerokuConfig
    }

data HerokuConfig = HerokuConfig
    { defaultPort          :: Int
    , herokuExecutableName :: String
    , herokuAppName        :: Maybe String
    }

instance Default HerokuConfig where
    def = HerokuConfig 3000 "heroku" Nothing

initHeroku :: MonadIO m => HerokuConfig -> Initializer' m Heroku
initHeroku conf = initializer' . liftIO $
    Heroku <$> newIORef Nothing <*> pure conf

-- | use this function instead of serverWith in heroku app. since 0.17.0.
--
-- @ serverWith exts (run 3000) . runApiary def $ foo @
--
-- to
--
-- @ herokuWith exts run def . runApiary def $ foo @
--
herokuWith :: MonadIO m => Initializer m '[Heroku] exts
           -> (Int -> Application -> m a)
           -> HerokuConfig -> EApplication exts m -> m a
herokuWith ir run conf eapp = ir' NoExtension $ \exts -> do
    port <- liftIO $ fmap read (getEnv "PORT")
        `catch` (\(_::IOError) -> return $ defaultPort conf)
    app  <- eapp exts
    run port app
  where
    Initializer ir' = initHeroku conf +> ir

-- | use this function instead of server in heroku app. since 0.17.0.
--
-- @ server (run 3000) . runApiary def $ foo @
--
-- to
--
-- @ heroku run def . runApiary def $ foo @
--
-- this function provide:
--
-- * set port by PORT environment variable.
-- * getHerokuEnv function(get config from environment variable or @ heroku config @ command).
heroku :: MonadIO m => (Int -> Application -> m a)
       -> HerokuConfig -> EApplication '[Heroku] m -> m a
heroku = herokuWith noExtension

getHerokuEnv' :: T.Text -> Heroku -> IO (Maybe T.Text)
getHerokuEnv' key Heroku{..} = try (getEnv $ T.unpack key) >>= \case
    Right e                 -> return (Just $ T.pack e)
    Left (_::SomeException) -> readIORef herokuEnv >>= \case
        Just hm -> return $ H.lookup key hm
        Nothing -> do
            let args = ["config", "-s"] ++
                    maybe [] (\n -> ["--app", n]) (herokuAppName herokuConfig)
                cp   = proc (herokuExecutableName herokuConfig) args
            (_, Just hout, _, h) <- createProcess cp {std_out = CreatePipe}
            xc <- waitForProcess h
            if xc == ExitSuccess
            then do
                hm <- H.fromList . map (second T.tail . T.break  (== '=')) . T.lines
                    <$> T.hGetContents hout
                writeIORef herokuEnv (Just hm)
                return $ H.lookup key hm
            else Nothing <$ writeIORef herokuEnv (Just H.empty)


getHerokuEnv :: Has Heroku exts => T.Text -> Extensions exts -> IO (Maybe T.Text)
getHerokuEnv key exts = getHerokuEnv' key (getExtension Proxy exts)
