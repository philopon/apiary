{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

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
import Data.Aeson
#ifdef DefineMonadLoggerInstance
import Control.Monad.Logger
#endif

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

#ifdef DefineMonadLoggerInstance
instance MonadLogger m => MonadLogger (ActionT m) where
    monadLoggerLog loc src lv msg = lift $ monadLoggerLog loc src lv msg
#endif

getRequest :: Monad m => ActionT m Request
getRequest = ActionT ask

modifyState :: Monad m => (ActionState -> ActionState) -> ActionT m ()
modifyState f = ActionT . lift $ modify f

status :: Monad m => Status -> ActionT m ()
status st = modifyState (\s -> s { actionStatus = st } )

modifyHeader :: Monad m => (ResponseHeaders -> ResponseHeaders) -> ActionT m ()
modifyHeader f = modifyState (\s -> s {actionHeaders = f $ actionHeaders s } )

addHeader :: Monad m => Header -> ActionT m ()
addHeader h = modifyHeader (h:)

setHeaders :: Monad m => ResponseHeaders -> ActionT m ()
setHeaders hs = modifyHeader (const hs)

contentType :: Monad m => S.ByteString -> ActionT m ()
contentType c = modifyHeader (\h -> ("Content-Type", c) : filter (("Content-Type" /=) . fst) h)

file' :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file' f p = modifyState (\s -> s { actionBody = File f p } )

builder :: Monad m => Builder -> ActionT m ()
builder b = modifyState (\s -> s { actionBody = Builder b } )

lbs :: Monad m => L.ByteString -> ActionT m ()
lbs l = modifyState (\s -> s { actionBody = LBS l } )

source :: Monad m => Source IO (Flush Builder) -> ActionT m ()
source src = modifyState (\s -> s { actionBody = SRC src } )

-- | set body to j and set Content-Type to \"application/json\"
json :: (ToJSON j, Monad m) => j -> ActionT m ()
json j = do
    contentType "application/json"
    lbs $ encode j
