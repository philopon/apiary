{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.Apiary.Action.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Network.Wai
import Network.Wai.Parse
import Network.Mime
import Data.Default.Class
import Network.HTTP.Types
import Blaze.ByteString.Builder
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Conduit

data ApiaryConfig = ApiaryConfig
    { -- | call when no handler matched.
      notFound      :: Application
      -- | used unless call 'status' function.
    , defaultStatus :: Status
      -- | initial headers.
    , defaultHeader :: ResponseHeaders
      -- | used by 'Control.Monad.Apiary.Filter.root' filter.
    , rootPattern   :: [S.ByteString]
    , mimeType      :: FilePath -> S.ByteString
    }

instance Default ApiaryConfig where
    def = ApiaryConfig 
        { notFound = \_ -> return $ responseLBS status404 
            [("Content-Type", "text/plain")] "404 Page Notfound.\n"
        , defaultStatus = ok200
        , defaultHeader = []
        , rootPattern   = ["", "/", "/index.html", "/index.htm"]
        , mimeType      = defaultMimeLookup . T.pack
        }

data ActionState 
    = ActionState
        { actionStatus  :: Status
        , actionHeaders :: ResponseHeaders
        , actionBody    :: Body
        , actionReqBody :: Maybe ([Param], [File L.ByteString])
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

data Action a 
    = Continue a
    | Pass
    | Stop Response

newtype ActionT m a = ActionT { unActionT :: forall b. 
    ApiaryConfig
    -> Request
    -> ActionState
    -> (a -> ActionState -> m (Action b))
    -> m (Action b)
    }

instance Functor (ActionT m) where
    fmap f m = ActionT $ \conf req st cont ->
        unActionT m conf req st (\a s' -> s' `seq` cont (f a) s')

instance Applicative (ActionT m) where
    pure x = ActionT $ \_ _ st cont -> cont x st
    mf <*> ma = ActionT $ \conf req st cont ->
        unActionT mf conf req st  $ \f st'  ->
        unActionT ma conf req st' $ \a st'' ->
        st' `seq` st'' `seq` cont (f a) st''

instance Monad m => Monad (ActionT m) where
    return x = ActionT $ \_ _ st cont -> cont x st
    m >>= k  = ActionT $ \conf req st cont ->
        unActionT m conf req st $ \a st' ->
        st' `seq` unActionT (k a) conf req st' cont
    fail _ = ActionT $ \_ _ _ _ -> return Pass

instance MonadIO m => MonadIO (ActionT m) where
    liftIO m = ActionT $ \_ _ st cont ->
        liftIO m >>= \a -> cont a st

instance MonadTrans ActionT where
    lift m = ActionT $ \_ _ st cont ->
        m >>= \a -> cont a st

instance MonadThrow m => MonadThrow (ActionT m) where
    throwM e = ActionT $ \_ _ st cont ->
        throwM e >>= \a -> cont a st

runActionT :: Monad m => ActionT m a
           -> ApiaryConfig -> Request -> ActionState
           -> m (Action (a, ActionState))
runActionT m conf req st = unActionT m conf req st $ \a st' ->
    st' `seq` return (Continue (a, st'))

actionT :: Monad m 
        => (ApiaryConfig -> Request -> ActionState -> m (Action (a, ActionState)))
        -> ActionT m a
actionT f = ActionT $ \conf req st cont -> f conf req st >>= \case
    Pass             -> return Pass
    Stop s           -> return $ Stop s
    Continue (a,st') -> st' `seq` cont a st'

-- | n must be Monad, so cant be MFunctor.
hoistActionT :: (Monad m, Monad n)
             => (forall b. m b -> n b) -> ActionT m a -> ActionT n a
hoistActionT run m = actionT $ \c r s -> run (runActionT m c r s)

execActionT :: ApiaryConfig -> ActionT IO () -> Application
execActionT config m request = runActionT m config request resp >>= \case
        Pass           -> notFound config request
        Stop s         -> return s
        Continue (_,r) -> return $ actionStateToResponse r
  where
    resp = ActionState (defaultStatus config) (defaultHeader config) (LBS "") Nothing

instance (Monad m, Functor m) => Alternative (ActionT m) where
    empty = mzero
    (<|>) = mplus

instance Monad m => MonadPlus (ActionT m) where
    mzero = actionT $ \_ _ _ -> return Pass
    mplus m n = actionT $ \c r s -> runActionT m c r s >>= \case
        Continue a -> return $ Continue a
        Stop stp   -> return $ Stop stp
        Pass       -> runActionT n c r s

instance MonadBase b m => MonadBase b (ActionT m) where
    liftBase = liftBaseDefault

instance MonadTransControl ActionT where
    newtype StT ActionT a = StActionT { unStActionT :: Action (a, ActionState) }
    liftWith f = actionT $ \c r s -> 
        liftM (\a -> Continue (a,s)) (f $ \t -> liftM StActionT $ runActionT t c r s)
    restoreT m = actionT $ \_ _ _ -> liftM unStActionT m

instance MonadBaseControl b m => MonadBaseControl b (ActionT m) where
    newtype StM (ActionT m) a = StMT { unStMT :: ComposeSt ActionT m a }
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM unStMT

instance MonadReader r m => MonadReader r (ActionT m) where
    ask     = lift ask
    local f = hoistActionT $ local f

-- | stop handler and send current state. since 0.3.3.0.
stop :: Monad m => ActionT m a
stop = ActionT $ \_ _ s _ -> return $ Stop (actionStateToResponse s)

-- | stop with response. since 0.4.2.0.
stopWith :: Monad m => Response -> ActionT m a
stopWith a = ActionT $ \_ _ _ _ -> return $ Stop a

-- | get raw request. since 0.1.0.0.
getRequest :: Monad m => ActionT m Request
getRequest = ActionT $ \_ r s c -> c r s

getRequestBody :: MonadIO m => ActionT m ([Param], [File L.ByteString])
getRequestBody = ActionT $ \_ r s c -> case actionReqBody s of
    Just b  -> c b s
    Nothing -> do
        b <- liftIO $ parseRequestBody lbsBackEnd r
        c b s { actionReqBody = Just b }

-- | parse request body and return params. since 0.9.0.0.
getReqParams :: MonadIO m => ActionT m [Param]
getReqParams = fst <$> getRequestBody

-- | parse request body and return files. since 0.9.0.0.
getReqFiles :: MonadIO m => ActionT m [File L.ByteString]
getReqFiles = snd <$> getRequestBody

getConfig :: Monad m => ActionT m ApiaryConfig
getConfig = ActionT $ \c _ s cont -> cont c s

modifyState :: Monad m => (ActionState -> ActionState) -> ActionT m ()
modifyState f = ActionT $ \_ _ s c -> c () (f s)

-- | get all request headers. since 0.6.0.0.
getHeaders :: Monad m => ActionT m RequestHeaders
getHeaders = requestHeaders `liftM` getRequest

-- | set status code. since 0.1.0.0.
status :: Monad m => Status -> ActionT m ()
status st = modifyState (\s -> s { actionStatus = st } )

-- | modify response header. since 0.1.0.0.
modifyHeader :: Monad m => (ResponseHeaders -> ResponseHeaders) -> ActionT m ()
modifyHeader f = modifyState (\s -> s {actionHeaders = f $ actionHeaders s } )

-- | add response header. since 0.1.0.0.
addHeader :: Monad m => HeaderName -> S.ByteString -> ActionT m ()
addHeader h v = modifyHeader ((h,v):)

-- | set response headers. since 0.1.0.0.
setHeaders :: Monad m => ResponseHeaders -> ActionT m ()
setHeaders hs = modifyHeader (const hs)

-- | set content-type header.
-- if content-type header already exists, replace it. since 0.1.0.0.
contentType :: Monad m => S.ByteString -> ActionT m ()
contentType c = modifyHeader
    (\h -> ("Content-Type", c) : filter (("Content-Type" /=) . fst) h)

-- | redirect handler
--
-- set status and add location header. since 0.3.3.0.
--
-- rename from redirect in 0.6.2.0.
redirectWith :: Monad m
             => Status
             -> S.ByteString -- ^ Location redirect to
             -> ActionT m ()
redirectWith st url = do
    status st
    addHeader "location" url

--      HTTP/1.0            HTTP/1.1
-- 300                      MultipleChoices
-- 301  MovedPermanently    MovedPermanently
-- 302  MovedTemporarily    Found
-- 303                      SeeOther
-- 304  NotModified         NotModified
-- 305                      UseProxy
-- 307                      TemporaryRedirect

-- | redirect with 301 Moved Permanently. since 0.3.3.0.
redirectPermanently :: Monad m => S.ByteString -> ActionT m ()
redirectPermanently = redirectWith movedPermanently301

-- | redirect with:
--
-- 303 See Other (HTTP/1.1)  or
-- 302 Moved Temporarily (Other)
-- 
-- since 0.6.2.0.
redirect :: Monad m => S.ByteString -> ActionT m ()
redirect to = do
    v <- httpVersion <$> getRequest
    if v == http11
        then redirectWith seeOther303 to
        else redirectWith status302   to

-- | redirect with:
--
-- 307 Temporary Redirect (HTTP/1.1) or
-- 302 Moved Temporarily (Other)
--
-- since 0.3.3.0.
redirectTemporary :: Monad m => S.ByteString -> ActionT m ()
redirectTemporary to = do
    v <- httpVersion <$> getRequest
    if v == http11
        then redirectWith temporaryRedirect307 to
        else redirectWith status302            to

-- | set response body file content and detect Content-Type by extension. since 0.1.0.0.
file :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file f p = do
    mime <- mimeType <$> getConfig
    contentType (mime f)
    file' f p

-- | set response body file content, without set Content-Type. since 0.1.0.0.
file' :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file' f p = modifyState (\s -> s { actionBody = File f p } )

-- | set response body builder. since 0.1.0.0.
builder :: Monad m => Builder -> ActionT m ()
builder b = modifyState (\s -> s { actionBody = Builder b } )

-- | set response body lazy bytestring. since 0.1.0.0.
lbs :: Monad m => L.ByteString -> ActionT m ()
lbs l = modifyState (\s -> s { actionBody = LBS l } )

-- | set response body source. since 0.1.0.0.
source :: Monad m => Source IO (Flush Builder) -> ActionT m ()
source src = modifyState (\s -> s { actionBody = SRC src } )

{-# DEPRECATED redirectFound, redirectSeeOther "use redirect" #-}
-- | redirect with 302 Found. since 0.3.3.0.
redirectFound       :: Monad m => S.ByteString -> ActionT m ()
redirectFound       = redirectWith found302

-- | redirect with 303 See Other. since 0.3.3.0.
redirectSeeOther    :: Monad m => S.ByteString -> ActionT m ()
redirectSeeOther    = redirectWith seeOther303
