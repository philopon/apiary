{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Web.Apiary.PureScript.Internal where

import System.FilePath(takeDirectory, (</>))
import qualified System.FilePath.Glob as G
import qualified System.IO.UTF8 as U

import qualified Language.Haskell.TH as TH
import qualified Language.PureScript as P

import Control.Monad.Apiary.Action(ActionT, contentType, string, bytes)
import Control.Exception(Exception, throwIO, try)
import Control.Applicative((<$>))

import Web.Apiary(MonadIO(..))
import Data.Apiary.Extension(Extension)

import Data.Default.Class(Default(def))
import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Typeable(Typeable)
import qualified Data.HashMap.Strict as H
import qualified Text.Parsec.Error as P

import qualified Paths_apiary_purescript as Path

data PureScriptException
    = ParseError   P.ParseError
    | CompileError String
    deriving (Show, Typeable)
instance Exception PureScriptException

purescriptDatadir :: FilePath
purescriptDatadir = takeDirectory $(TH.stringE =<< TH.runIO Path.getDataDir) </> "purescript-" ++ VERSION_purescript

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
    , pureScriptOptions :: P.Options P.Compile
    }

instance Default PureScriptConfig where
    def = PureScriptConfig 
        defaultPatterns
        "."
        defaultPreludePath
        False
        []
        P.defaultCompileOptions
        { P.optionsMain             = Just "Main" }

data PureScript = PureScript
    { pscConfig :: PureScriptConfig
    , compiled  :: IORef (H.HashMap FilePath String)
    }
instance Extension PureScript

makePureScript :: MonadIO m => PureScriptConfig -> m PureScript
makePureScript conf = do
    ir <- liftIO $ mapM (\p -> (p,) <$> compile conf p) (initialCompiles conf)
    p  <- liftIO $ PureScript conf <$> newIORef (H.fromList ir)
    return p

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

compile :: PureScriptConfig -> FilePath -> IO String
compile opt p = do
    mods <- pscModules opt
    mn   <- readPscInput p
    case P.compile (pureScriptOptions opt) (mn ++ mods) [] of
        Left l           -> throwIO (CompileError l)
        Right (js, _, _) -> return js

pureScript :: MonadIO m => PureScript -> FilePath -> ActionT exts prms m ()
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
        Right r -> string r
        Left  e | development (pscConfig env) -> do
            bytes "console.log(\""
            string $ pr (e :: PureScriptException)
            bytes "\")"
                | otherwise -> bytes "console.log(\"PureScript error.\");"
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
