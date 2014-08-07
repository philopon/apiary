{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.Apiary.Action.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans.Control

import Network.Mime
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Parse as P

import Data.Apiary.Param
import Data.Apiary.Document
import Data.Apiary.Document.Html
import Data.Default.Class

import Blaze.ByteString.Builder
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T

#ifndef WAI3
import Data.Conduit
type StreamingBody = Source IO (Flush Builder)
#endif

data ApiaryConfig = ApiaryConfig
    { -- | call when no handler matched.
      notFound            :: Application
      -- | used unless call 'status' function.
    , defaultStatus       :: Status
      -- | initial headers.
    , defaultHeaders      :: ResponseHeaders
    , failStatus          :: Status
    , failHeaders         :: ResponseHeaders
      -- | used by 'Control.Monad.Apiary.Filter.root' filter.
    , rootPattern         :: [T.Text]
    , mimeType            :: FilePath -> S.ByteString
    }

defaultDocumentationAction :: Monad m => DefaultDocumentConfig -> ActionT m ()
defaultDocumentationAction conf = do
    d <- getDocuments
    contentType "text/html"
    builder . renderHtmlBuilder $ defaultDocumentToHtml conf d

defaultNotFound :: Application
#ifdef WAI3
defaultNotFound _ f = f      $ responseLBS status404 [("Content-Type", "text/plain")] "404 Page Notfound.\n"
#else
defaultNotFound _   = return $ responseLBS status404 [("Content-Type", "text/plain")] "404 Page Notfound.\n"
#endif

instance Default ApiaryConfig where
    def = ApiaryConfig 
        { notFound            = defaultNotFound
        , defaultStatus       = ok200
        , defaultHeaders      = []
        , failStatus          = internalServerError500
        , failHeaders         = []
        , rootPattern         = ["", "/", "index.html", "index.htm"]
        , mimeType            = defaultMimeLookup . T.pack
        }

--------------------------------------------------------------------------------

data ActionState = ActionState
    { actionResponse :: Response
    , actionStatus   :: Status
    , actionHeaders  :: ResponseHeaders
    , actionReqBody  :: Maybe ([Param], [File])
    , actionFetches  :: [T.Text]
    }

initialState :: ApiaryConfig -> ActionState
initialState conf = ActionState
    { actionResponse = responseLBS (defaultStatus conf) (defaultHeaders conf) ""
    , actionStatus   = defaultStatus  conf
    , actionHeaders  = defaultHeaders conf
    , actionReqBody  = Nothing
    , actionFetches  = []
    }
{-# INLINE initialState #-}

--------------------------------------------------------------------------------

data ActionEnv = ActionEnv
    { actionConfig    :: ApiaryConfig
    , actionRequest   :: Request
    , actionDocuments :: Documents
    }

data Action a 
    = Continue ActionState a
    | Pass
    | Stop Response
    deriving (Functor)

newtype ActionT m a = ActionT { runActionT ::
    ActionEnv
    -> ActionState
    -> m (Action a)
    }

-- | n must be Monad, so cant be MFunctor.
hoistActionT :: (forall b. m b -> n b) -> ActionT m a -> ActionT n a
hoistActionT run m = ActionT $ \e s -> run (runActionT m e s)
{-# INLINE hoistActionT #-}

execActionT :: ApiaryConfig -> Documents -> ActionT IO () -> Application
#ifdef WAI3
execActionT config doc m request send = 
#else
execActionT config doc m request = let send = return in
#endif
    runActionT m (ActionEnv config request doc) (initialState config) >>= \case
#ifdef WAI3
        Pass           -> notFound config request send
#else
        Pass           -> notFound config request
#endif
        Stop s         -> send s
        Continue r _   -> send $ actionResponse r

--------------------------------------------------------------------------------

instance Functor m => Functor (ActionT m) where
    fmap f m = ActionT $ \env st ->
        fmap f <$> runActionT m env st

instance (Functor m, Monad m) => Applicative (ActionT m) where
    pure x = ActionT $ \_ s -> return $ Continue s x
    mf <*> ma = ActionT $ \env st ->
        runActionT mf env st >>= \case
            Pass   -> return Pass
            Stop r -> return $ Stop r
            Continue st' f -> runActionT ma env st' >>= \case
                Continue st'' a -> return $ Continue st'' (f a)
                Pass   -> return Pass
                Stop r -> return $ Stop r
            
instance Monad m => Monad (ActionT m) where
    return x = ActionT $ \_ st -> return $ Continue st x
    m >>= k  = ActionT $ \env st ->
        runActionT m env st >>= \case
            Pass   -> return Pass
            Stop r -> return $ Stop r
            Continue st' a -> runActionT (k a) env st' >>= \case
                Pass   -> return Pass
                Stop r -> return $ Stop r
                Continue st'' b -> return $ Continue st'' b
    fail s = ActionT $ \ActionEnv{actionConfig = c} _ ->
        return $ Stop (responseLBS (failStatus c) (failHeaders c) $ LC.pack s)

instance MonadIO m => MonadIO (ActionT m) where
    liftIO m = ActionT $ \_ st ->
        Continue st `liftM` liftIO m

instance MonadTrans ActionT where
    lift m = ActionT $ \_ st ->
        Continue st `liftM` m

instance MonadThrow m => MonadThrow (ActionT m) where
    throwM e = ActionT $ \_ st ->
        Continue st `liftM` throwM e

instance MonadCatch m => MonadCatch (ActionT m) where
    catch m h = ActionT $ \env st -> 
        catch (runActionT m env st) (\e -> runActionT (h e) env st)
    {-# INLINE catch #-}

instance MonadMask m => MonadMask (ActionT m) where
    mask a = ActionT $ \env st ->
        mask $ \u -> runActionT (a $ q u) env st
      where
        q u m = ActionT $ \env st -> u (runActionT m env st)
    uninterruptibleMask a = ActionT $ \env st ->
        uninterruptibleMask $ \u -> runActionT (a $ q u) env st
      where
        q u m = ActionT $ \env st -> u (runActionT m env st)
    {-# INLINE mask #-}
    {-# INLINE uninterruptibleMask #-}

instance (Monad m, Functor m) => Alternative (ActionT m) where
    empty = mzero
    (<|>) = mplus
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance Monad m => MonadPlus (ActionT m) where
    mzero = ActionT $ \_ _ -> return Pass
    mplus m n = ActionT $ \e s -> runActionT m e s >>= \case
        Continue st a -> return $ Continue st a
        Stop stp      -> return $ Stop stp
        Pass          -> runActionT n e s
    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadBase b m => MonadBase b (ActionT m) where
    liftBase = liftBaseDefault

instance MonadTransControl ActionT where
    newtype StT ActionT a = StActionT { unStActionT :: Action a }
    liftWith f = ActionT $ \e s -> 
        liftM (\a -> Continue s a) (f $ \t -> liftM StActionT $ runActionT t e s)
    restoreT m = ActionT $ \_ _ -> liftM unStActionT m

instance MonadBaseControl b m => MonadBaseControl b (ActionT m) where
    newtype StM (ActionT m) a = StMT { unStMT :: ComposeSt ActionT m a }
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM unStMT

instance MonadReader r m => MonadReader r (ActionT m) where
    ask     = lift ask
    local f = hoistActionT $ local f

--------------------------------------------------------------------------------

getEnv :: Monad m => ActionT m ActionEnv
getEnv = ActionT $ \e st -> return $ Continue st e

-- | get raw request. since 0.1.0.0.
getRequest :: Monad m => ActionT m Request
getRequest = liftM actionRequest getEnv

getConfig :: Monad m => ActionT m ApiaryConfig
getConfig = liftM actionConfig getEnv

getDocuments :: Monad m => ActionT m Documents
getDocuments = liftM actionDocuments getEnv

getRequestBody :: MonadIO m => ActionT m ([Param], [File])
getRequestBody = ActionT $ \e s -> case actionReqBody s of
    Just b  -> return $ Continue s b
    Nothing -> do
        (p,f) <- liftIO $ P.parseRequestBody P.lbsBackEnd (actionRequest e)
        let b = (p, map convFile f)
        return $ Continue s { actionReqBody = Just b } b
  where
    convFile (p, P.FileInfo{..}) = File p fileName fileContentType fileContent

-- | parse request body and return params. since 0.9.0.0.
getReqParams :: MonadIO m => ActionT m [Param]
getReqParams = fst `liftM` getRequestBody

-- | parse request body and return files. since 0.9.0.0.
getReqFiles :: MonadIO m => ActionT m [File]
getReqFiles = snd `liftM` getRequestBody

--------------------------------------------------------------------------------

modifyState :: Monad m => (ActionState -> ActionState) -> ActionT m ()
modifyState f = ActionT $ \_ s -> return $ Continue (f s) ()

getState :: Monad m => ActionT m ActionState
getState = ActionT $ \_ s -> return $ Continue s s

-- | set status code. since 0.1.0.0.
status :: Monad m => Status -> ActionT m ()
status st = modifyState (\s -> s { actionStatus = st } )

-- | get all request headers. since 0.6.0.0.
getHeaders :: Monad m => ActionT m RequestHeaders
getHeaders = requestHeaders `liftM` getRequest

-- | modify response header. since 0.1.0.0.
modifyHeader :: Monad m => (ResponseHeaders -> ResponseHeaders) -> ActionT m ()
modifyHeader f = modifyState (\s -> s {actionHeaders = f $ actionHeaders s } )

-- | add response header. since 0.1.0.0.
addHeader :: Monad m => HeaderName -> S.ByteString -> ActionT m ()
addHeader h v = modifyHeader ((h,v):)

-- | set response headers. since 0.1.0.0.
setHeaders :: Monad m => ResponseHeaders -> ActionT m ()
setHeaders hs = modifyHeader (const hs)

type ContentType = S.ByteString

-- | set content-type header.
-- if content-type header already exists, replace it. since 0.1.0.0.
contentType :: Monad m => ContentType -> ActionT m ()
contentType c = modifyHeader
    (\h -> ("Content-Type", c) : filter (("Content-Type" /=) . fst) h)

--------------------------------------------------------------------------------

-- | stop handler and send current state. since 0.3.3.0.
stop :: Monad m => ActionT m a
stop = ActionT $ \_ s -> return $ Stop (actionResponse s)

-- | stop with response. since 0.4.2.0.
stopWith :: Monad m => Response -> ActionT m a
stopWith a = ActionT $ \_ _ -> return $ Stop a

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
    v <- httpVersion `liftM` getRequest
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
    v <- httpVersion `liftM` getRequest
    if v == http11
        then redirectWith temporaryRedirect307 to
        else redirectWith status302            to

-- | Raw response constructor. since 0.10.
--
-- example(use pipes-wai)
--
-- @
-- producer :: Monad m => Producer (Flush Builder) IO () -> ActionT m ()
-- producer = response (\s h -> responseProducer s h)
-- @
--
rawResponse :: Monad m => (Status -> ResponseHeaders -> Response) -> ActionT m ()
rawResponse f = modifyState (\s -> s { actionResponse = f (actionStatus s) (actionHeaders s)} )

-- | set response body file content, without set Content-Type. since 0.1.0.0.
file' :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file' f p = rawResponse (\s h -> responseFile s h f p)

-- | set response body file content and detect Content-Type by extension. since 0.1.0.0.
file :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file f p = do
    mime <- mimeType `liftM` getConfig
    contentType (mime f)
    file' f p

-- | set response body builder. since 0.1.0.0.
builder :: Monad m => Builder -> ActionT m ()
builder b = rawResponse (\s h -> responseBuilder s h b)

-- | set response body lazy bytestring. since 0.1.0.0.
lbs :: Monad m => L.ByteString -> ActionT m ()
lbs l = rawResponse (\s h -> responseLBS s h l)

-- | set response body source. since 0.9.0.0.
stream :: Monad m => StreamingBody -> ActionT m ()
#ifdef WAI3
stream str = rawResponse (\s h -> responseStream s h str)
#else
stream str = rawResponse (\s h -> responseSource s h str)
#endif

{-# DEPRECATED source "use stream" #-}
source :: Monad m => StreamingBody -> ActionT m ()
source = stream

{-# DEPRECATED redirectFound, redirectSeeOther "use redirect" #-}
-- | redirect with 302 Found. since 0.3.3.0.
redirectFound       :: Monad m => S.ByteString -> ActionT m ()
redirectFound       = redirectWith found302

-- | redirect with 303 See Other. since 0.3.3.0.
redirectSeeOther    :: Monad m => S.ByteString -> ActionT m ()
redirectSeeOther    = redirectWith seeOther303
