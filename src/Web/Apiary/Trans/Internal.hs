{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Web.Apiary.Trans.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Network.Wai
import Data.Default
import Data.Monoid
import Network.HTTP.Types
import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import Data.Conduit

data ApiaryConfig m = ApiaryConfig
    { notFound      :: ApplicationM m 
    , defaultStatus :: Status
    , defaultHeader :: ResponseHeaders
    }

instance Monad m => Default (ApiaryConfig m) where
    def = ApiaryConfig 
        { notFound = \_ -> return $ responseLBS status404 
            [("Content-Type", "text/plain")] "404 Page Notfound."
        , defaultStatus = ok200
        , defaultHeader = []
        }

type ApplicationM m = Request -> m Response

data ActionState = ActionState
    { actionStatus  :: Status
    , actionHeaders :: ResponseHeaders
    , actionBody    :: Body
    }

data Body 
    = File FilePath (Maybe FilePart)
    | Builder Builder
    | LBS L.ByteString
    | SRC (Source IO (Flush Builder))

actionStateToResponse :: ActionState -> Response
actionStateToResponse as = case actionBody as of
    File f p  -> responseFile st hd f p
    Builder b -> responseBuilder st hd b
    LBS l     -> responseLBS st hd l
    SRC    s  -> responseSource st hd s
  where
    st = actionStatus  as
    hd = actionHeaders as

newtype ActionT m a = ActionT
    { unActionT :: ReaderT Request (StateT ActionState (MaybeT m)) a 
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans ActionT where
    lift = ActionT . lift . lift . lift

runActionT :: Monad m => ApiaryConfig m -> ActionT m () -> (ApplicationM m)
runActionT config (ActionT m) request =
    runMaybeT (runStateT (runReaderT m request) resp) >>= \case
        Nothing    -> notFound config request
        Just (_,r) -> return $ actionStateToResponse r
  where
    resp = ActionState (defaultStatus config) (defaultHeader config) (LBS "")

instance (Monad m, Functor m) => Alternative (ActionT m) where
    empty = mzero
    (<|>) = mplus

instance Monad m => MonadPlus (ActionT m) where
    mzero = ActionT . ReaderT $ \_ -> StateT $ \_ -> MaybeT (return Nothing)
    mplus (ActionT m) (ActionT n) = ActionT . ReaderT $ \r -> StateT $ \s ->
        MaybeT $ runMaybeT (runStateT (runReaderT m r) s) >>= \case
            Just a  -> return $ Just a
            Nothing -> runMaybeT (runStateT (runReaderT n r) s)

instance Monad m => Monoid (ActionT m ()) where
    mempty  = mzero
    mappend = mplus

getRequest :: Monad m => ActionT m Request
getRequest = ActionT ask

status :: Monad m => Status -> ActionT m ()
status st = ActionT . lift $ modify (\s -> s { actionStatus = st } )

addHeader :: Monad m => Header -> ActionT m ()
addHeader h = ActionT . lift $ modify (\s -> s { actionHeaders = h : actionHeaders s } )

file :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file f p = ActionT . lift $ modify (\s -> s { actionBody = File f p } )

builder :: Monad m => Builder -> ActionT m ()
builder b = ActionT . lift $ modify (\s -> s { actionBody = Builder b } )

lbs :: Monad m => L.ByteString -> ActionT m ()
lbs l = ActionT . lift $ modify (\s -> s { actionBody = LBS l } )

source :: Monad m => Source IO (Flush Builder) -> ActionT m ()
source src = ActionT . lift $ modify (\s -> s { actionBody = SRC src } )

method :: Monad m => Method -> ActionT m b -> ActionT m b
method m a = getRequest >>= \r -> if requestMethod r == m then a else mzero

stdMethod :: Monad m => StdMethod -> ActionT m b -> ActionT m b
stdMethod m a = getRequest >>= \r -> if requestMethod r == renderStdMethod m then a else mzero

newtype ApiaryT m a = ApiaryM { unApiaryT :: Writer (ActionT m ()) a }
    deriving (Functor, Applicative, Monad)

runApiaryT :: Monad m => ApiaryConfig m -> ApiaryT m a -> ApplicationM m
runApiaryT config (ApiaryM m) = runActionT config (execWriter m)

addRoute :: Monad m => ActionT m () -> ApiaryT m ()
addRoute = ApiaryM . tell

function :: Monad m => (Request -> Bool) -> ActionT m () -> ApiaryT m ()
function f a = addRoute $ getRequest >>= \r -> if f r then a else mempty
