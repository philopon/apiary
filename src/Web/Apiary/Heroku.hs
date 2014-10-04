{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Apiary.Heroku
    ( Heroku
    -- * configuration
    , HerokuConfig(..)
    -- * runner
    , runHeroku, runHerokuWith, runHerokuTWith
    -- * extension functions
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
import Data.Apiary.Compat
import Data.Default.Class
import qualified Data.HashMap.Strict as H
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Network.Wai
import Control.Monad.Apiary
import Data.Apiary.Extension

data Heroku = Heroku 
    { herokuEnv    :: IORef (Maybe (H.HashMap T.Text T.Text))
    , herokuConfig :: HerokuConfig
    }

data HerokuConfig = HerokuConfig
    { defaultPort          :: Int
    , herokuExecutableName :: String
    , herokuAppName        :: Maybe String
    , herokuApiaryConfig   :: ApiaryConfig
    }

instance Default HerokuConfig where
    def = HerokuConfig 3000 "heroku" Nothing def

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
runHerokuTWith :: (MonadIO m, Monad actM)
               => (forall b. actM b -> IO b)
               -> (Int -> Application -> m a)
               -> Initializer m '[Heroku] exts
               -> HerokuConfig
               -> ApiaryT exts '[] actM m ()
               -> m a
runHerokuTWith runAct run ir conf m = do
    port <- liftIO $ fmap read (getEnv "PORT")
        `catch` (\(_::IOError) -> return $ defaultPort conf)
    runApiaryTWith runAct (run port) (initHeroku conf +> ir) (herokuApiaryConfig conf) m

runHerokuWith :: MonadIO m
              => (Int -> Application -> m a)
              -> Initializer m '[Heroku] exts
              -> HerokuConfig
              -> ApiaryT exts '[] IO m ()
              -> m a
runHerokuWith = runHerokuTWith id

-- | use this function instead of runApiary in heroku app. since 0.18.0.
--
-- this function provide:
--
-- * set port by PORT environment variable.
-- * getHerokuEnv function(get config from environment variable or @ heroku config @ command).
runHeroku :: MonadIO m
          => (Int -> Application -> m a)
          -> HerokuConfig
          -> ApiaryT '[Heroku] '[] IO m ()
          -> m a
runHeroku run = runHerokuWith run noExtension

getHerokuEnv' :: T.Text -- ^ heroku environment variable name
              -> Heroku -> IO (Maybe T.Text)
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


getHerokuEnv :: Has Heroku exts => T.Text -- ^ heroku environment variable name
             -> Extensions exts -> IO (Maybe T.Text)
getHerokuEnv key exts = getHerokuEnv' key (getExtension Proxy exts)
