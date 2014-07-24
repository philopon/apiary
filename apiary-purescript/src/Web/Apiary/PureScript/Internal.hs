{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.Apiary.PureScript.Internal where

import System.FilePath
import qualified System.FilePath.Glob as G
import qualified System.IO.UTF8 as U

import Language.Haskell.TH
import qualified Language.PureScript as P

import Control.Exception
import Control.Applicative

import Web.Apiary

import Data.IORef
import Data.Typeable
import qualified Data.HashMap.Strict as H
import qualified Text.Parsec.Error as P
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import qualified Paths_apiary_purescript as Path

data PureScriptException
    = ParseError   P.ParseError
    | CompileError String
    deriving (Show, Typeable)
instance Exception PureScriptException

purescriptDatadir :: FilePath
purescriptDatadir = takeDirectory $(stringE =<< runIO Path.getDataDir) </> "purescript-" ++ VERSION_purescript

defaultPatterns :: [G.Pattern]
defaultPatterns = map G.compile 
    [ "src/**/*.purs"
    , "bower_components/purescript-*/src/**/*.purs"
    , "bower_components/purescript-*/src/**/*.purs.hs"
    ]

data PureScriptConfig = PureScriptConfig
    { libraryPatterns   :: [G.Pattern]
    , libraryBaseDir    :: FilePath
    , preludePath       :: FilePath
    , development       :: Bool
    , initialCompiles   :: [FilePath]
    , pureScriptOptions :: P.Options
    }

instance Default PureScriptConfig where
    def = PureScriptConfig 
        defaultPatterns
        "."
        defaultPreludePath
        False
        []
        P.defaultOptions
        { P.optionsMain             = Just "Main"
        , P.optionsBrowserNamespace = Just "PS"
        }

data PureScript = PureScript
    { pscConfig :: PureScriptConfig
    , compiled  :: IORef (H.HashMap FilePath L.ByteString)
    }

withPureScript :: MonadIO m => PureScriptConfig -> (PureScript -> m b) -> m b
withPureScript conf m = do
    ir <- liftIO $ mapM (\p -> (p,) <$> compile conf p) (initialCompiles conf)
    p  <- liftIO $ PureScript conf <$> newIORef (H.fromList ir)
    m p

defaultPreludePath :: FilePath
defaultPreludePath = purescriptDatadir </> "prelude/prelude.purs"

readPscInput :: FilePath -> IO [P.Module]
readPscInput p = do
    txt <- U.readFile p
    case P.runIndentParser p P.parseModules txt of
        Left e  -> throwIO $ ParseError e
        Right r -> return r

pscModules :: PureScriptConfig -> IO [P.Module]
pscModules conf = do
    mods <- liftIO $ do
        concat . fst <$> G.globDir (libraryPatterns conf) (libraryBaseDir conf)
    let prel = preludePath conf
    concat `fmap` mapM readPscInput (prel : mods)

compile :: PureScriptConfig -> FilePath -> IO L.ByteString
compile opt p = do
    mods <- pscModules opt
    mn   <- readPscInput p
    case P.compile (pureScriptOptions opt) $ mn ++ mods of
        Left l           -> throwIO (CompileError l)
        Right (js, _, _) -> return . T.encodeUtf8 $ T.pack js

pureScript :: MonadIO m => PureScript -> FilePath -> ActionT m ()
pureScript env p = do
    contentType "text/javascript"
    s <- liftIO . try $ 
        if development (pscConfig env)
        then compile (pscConfig env) p
        else (H.lookup p <$> readIORef (compiled env)) >>= \case
           Nothing -> do
               r <- compile (pscConfig env) p
               atomicModifyIORef' (compiled env) ((,()) . H.insert p r)
               return r
           Just r  -> return r
    case s of
        Right r -> lbs r
        Left  e | development (pscConfig env) -> 
            lbs . LC.pack $ "alert(\"" ++ pr (e::PureScriptException) ++ "\")"

                | otherwise -> lbs "alert(\"PureScript error.\");"
  where
    pr = concatMap esc . show
    esc '"'  = "\\\""
    esc '\'' = "\\'"
    esc '\\' = "\\\\"
    esc '/'  = "\\/"
    esc '<'  = "\\x3c"
    esc '>'  = "\\x3e"
    esc '\n' = "\\n"
    esc c    = [c]
