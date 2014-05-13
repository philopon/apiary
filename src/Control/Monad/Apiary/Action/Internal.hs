{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Apiary.Action.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control
import Network.Wai
import Data.Default
import Data.Monoid
import Network.HTTP.Types
import Blaze.ByteString.Builder
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit

data ApiaryConfig m = ApiaryConfig
    { -- | call when no handler matched.
      notFound      :: ApplicationM m 
      -- | used unless call 'status' function.
    , defaultStatus :: Status
      -- | initial headers.
    , defaultHeader :: ResponseHeaders
      -- | used by 'Control.Monad.Apiary.Filter.root' filter.
    , rootPattern   :: [S.ByteString]
    }

instance Monad m => Default (ApiaryConfig m) where
    def = ApiaryConfig 
        { notFound = \_ -> return $ responseLBS status404 
            [("Content-Type", "text/plain")] "404 Page Notfound."
        , defaultStatus = ok200
        , defaultHeader = []
        , rootPattern   = ["", "/", "/index.html", "/index.htm"]
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

instance MonadBase b m => MonadBase b (ActionT m) where
    liftBase = liftBaseDefault

instance MonadTransControl ActionT where
    newtype StT ActionT a = StAction { unStAction :: StT MaybeT (StT (StateT ActionState) (StT (ReaderT Request) a)) }
    liftWith f = ActionT $ liftWith $ \run -> liftWith $ \run' -> liftWith $ \run'' -> 
        f $ liftM StAction . run'' . run' . run . unActionT
    restoreT = ActionT . restoreT . restoreT . restoreT . liftM unStAction

instance MonadBaseControl b m => MonadBaseControl b (ActionT m) where
    newtype StM (ActionT m) a = StMT { unStMT :: ComposeSt ActionT m a }
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM   unStMT

getRequest :: Monad m => ActionT m Request
getRequest = ActionT ask

status :: Monad m => Status -> ActionT m ()
status st = ActionT . lift $ modify (\s -> s { actionStatus = st } )

addHeader :: Monad m => Header -> ActionT m ()
addHeader h = ActionT . lift $ modify (\s -> s { actionHeaders = h : actionHeaders s } )

setHeaders :: Monad m => ResponseHeaders -> ActionT m ()
setHeaders hs = ActionT . lift $ modify (\s -> s { actionHeaders = hs } )

file' :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file' f p = ActionT . lift $ modify (\s -> s { actionBody = File f p } )

builder :: Monad m => Builder -> ActionT m ()
builder b = ActionT . lift $ modify (\s -> s { actionBody = Builder b } )

lbs :: Monad m => L.ByteString -> ActionT m ()
lbs l = ActionT . lift $ modify (\s -> s { actionBody = LBS l } )

source :: Monad m => Source IO (Flush Builder) -> ActionT m ()
source src = ActionT . lift $ modify (\s -> s { actionBody = SRC src } )

