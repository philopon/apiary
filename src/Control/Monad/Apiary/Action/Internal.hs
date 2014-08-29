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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.Apiary.Action.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans.Control

import Network.Mime hiding (Extension)
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Parse as P

import Data.Monoid hiding (All)
import Data.Apiary.Extension
import Data.Apiary.Param
import Data.Apiary.Document
import Data.Apiary.Document.Html
import Data.Default.Class

import Blaze.ByteString.Builder
import Text.Blaze.Html.Renderer.Utf8
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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

defaultDocumentationAction :: Monad m => DefaultDocumentConfig -> ActionT exts m ()
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
        , rootPattern         = ["index.html", "index.htm"]
        , mimeType            = defaultMimeLookup . T.pack
        }

--------------------------------------------------------------------------------

data ResponseBody
    = ResponseFile FilePath (Maybe FilePart)
    | ResponseBuilder Builder
    | ResponseStream StreamingBody
    | ResponseRaw (IO S.ByteString -> (S.ByteString -> IO ()) -> IO ()) Response
    | ResponseFunc (Status -> ResponseHeaders -> Response)

instance Monoid ResponseBody where
    mempty = ResponseBuilder mempty
    ResponseBuilder a `mappend` ResponseBuilder b = ResponseBuilder $ a <> b
    _ `mappend` b = b


toResponse :: ActionState -> Response
toResponse ActionState{..} = case actionResponse of
    ResponseFile  f p -> responseFile    actionStatus actionHeaders f p
    ResponseBuilder b -> responseBuilder actionStatus actionHeaders b
#ifdef WAI3
    ResponseStream  s -> responseStream  actionStatus actionHeaders s
#else
    ResponseStream  s -> responseSource  actionStatus actionHeaders s
#endif
    ResponseRaw   f r -> responseRaw f r
    ResponseFunc    f -> f actionStatus actionHeaders

data ActionState = ActionState
    { actionResponse :: ResponseBody
    , actionStatus   :: Status
    , actionHeaders  :: ResponseHeaders
    , actionReqBody  :: Maybe ([Param], [File])
    , actionFetches  :: [T.Text]
    }

initialState :: ApiaryConfig -> ActionState
initialState conf = ActionState
    { actionResponse = ResponseBuilder mempty
    , actionStatus   = defaultStatus  conf
    , actionHeaders  = defaultHeaders conf
    , actionReqBody  = Nothing
    , actionFetches  = []
    }
{-# INLINE initialState #-}

--------------------------------------------------------------------------------

data ActionEnv exts = ActionEnv
    { actionConfig    :: ApiaryConfig
    , actionRequest   :: Request
    , actionDocuments :: Documents
    , actionExts      :: Extensions exts
    }

data Action a 
    = Continue ActionState a
    | Pass
    | Stop Response

newtype ActionT exts m a = ActionT { unActionT :: forall b. 
    ActionEnv exts
    -> ActionState
    -> (a -> ActionState -> m (Action b))
    -> m (Action b)
    }

runActionT :: Monad m => ActionT exts m a
           -> ActionEnv exts -> ActionState
           -> m (Action a)
runActionT m env st = unActionT m env st $ \a !st' ->
    return (Continue st' a)
{-# INLINE runActionT #-}

actionT :: Monad m 
        => (ActionEnv exts -> ActionState -> m (Action a))
        -> ActionT exts m a
actionT f = ActionT $ \env !st cont -> f env st >>= \case
    Pass            -> return Pass
    Stop s          -> return $ Stop s
    Continue !st' a -> cont a st'
{-# INLINE actionT #-}

-- | n must be Monad, so cant be MFunctor.
hoistActionT :: (Monad m, Monad n)
             => (forall b. m b -> n b) -> ActionT exts m a -> ActionT exts n a
hoistActionT run m = actionT $ \e s -> run (runActionT m e s)
{-# INLINE hoistActionT #-}

execActionT :: ApiaryConfig -> Extensions exts -> Documents -> ActionT exts IO () -> Application
#ifdef WAI3
execActionT config exts doc m request send = 
#else
execActionT config exts doc m request = let send = return in
#endif
    runActionT m (ActionEnv config request doc exts) (initialState config) >>= \case
#ifdef WAI3
        Pass         -> notFound config request send
#else
        Pass         -> notFound config request
#endif
        Stop s       -> send s
        Continue r _ -> send $ toResponse r

--------------------------------------------------------------------------------

instance Functor (ActionT exts m) where
    fmap f m = ActionT $ \env st cont ->
        unActionT m env st (\a !s' -> cont (f a) s')

instance Applicative (ActionT exts m) where
    pure x = ActionT $ \_ !st cont -> cont x st
    mf <*> ma = ActionT $ \env st cont ->
        unActionT mf env st  $ \f !st'  ->
        unActionT ma env st' $ \a !st'' ->
        cont (f a) st''

instance Monad m => Monad (ActionT exts m) where
    return x = ActionT $ \_ !st cont -> cont x st
    m >>= k  = ActionT $ \env !st cont ->
        unActionT m env st $ \a !st' ->
        unActionT (k a) env st' cont
    fail s = ActionT $ \(ActionEnv{actionConfig = c}) _ _ -> return $
        Stop (responseLBS (failStatus c) (failHeaders c) $ LC.pack s)

instance MonadIO m => MonadIO (ActionT exts m) where
    liftIO m = ActionT $ \_ !st cont ->
        liftIO m >>= \a -> cont a st

instance MonadTrans (ActionT exts) where
    lift m = ActionT $ \_ !st cont ->
        m >>= \a -> cont a st

instance MonadThrow m => MonadThrow (ActionT exts m) where
    throwM e = ActionT $ \_ !st cont ->
        throwM e >>= \a -> cont a st

instance MonadCatch m => MonadCatch (ActionT exts m) where
    catch m h = ActionT $ \env !st cont ->
        catch (unActionT m env st cont) (\e -> unActionT (h e) env st cont)
    {-# INLINE catch #-}

instance MonadMask m => MonadMask (ActionT exts m) where
    mask a = ActionT $ \env !st cont ->
        mask $ \u -> unActionT (a $ q u) env st cont
      where
        q u m = actionT $ \env !st -> u (runActionT m env st)
    uninterruptibleMask a = ActionT $ \env !st cont ->
        uninterruptibleMask $ \u -> unActionT (a $ q u) env st cont
      where
        q u m = actionT $ \env !st -> u (runActionT m env st)
    {-# INLINE mask #-}
    {-# INLINE uninterruptibleMask #-}

instance (Monad m, Functor m) => Alternative (ActionT exts m) where
    empty = mzero
    (<|>) = mplus
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance Monad m => MonadPlus (ActionT exts m) where
    mzero = ActionT $ \_ _ _ -> return Pass
    mplus m n = ActionT $ \e !s cont -> unActionT m e s cont >>= \case
        Continue !st a -> return $ Continue st a
        Stop stp       -> return $ Stop stp
        Pass           -> unActionT n e s cont
    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadBase b m => MonadBase b (ActionT exts m) where
    liftBase = liftBaseDefault

instance MonadTransControl (ActionT exts) where
    newtype StT (ActionT exts) a = StActionT { unStActionT :: Action a }
    liftWith f = actionT $ \e !s -> 
        liftM (\a -> Continue s a) (f $ \t -> liftM StActionT $ runActionT t e s)
    restoreT m = actionT $ \_ _ -> liftM unStActionT m

instance MonadBaseControl b m => MonadBaseControl b (ActionT exts m) where
    newtype StM (ActionT exts m) a = StMT { unStMT :: ComposeSt (ActionT exts) m a }
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM unStMT

instance MonadReader r m => MonadReader r (ActionT exts m) where
    ask     = lift ask
    local f = hoistActionT $ local f

--------------------------------------------------------------------------------

getEnv :: Monad m => ActionT exts m (ActionEnv exts)
getEnv = ActionT $ \e s c -> c e s

-- | get raw request. since 0.1.0.0.
getRequest :: Monad m => ActionT exts m Request
getRequest = liftM actionRequest getEnv

getConfig :: Monad m => ActionT exts m ApiaryConfig
getConfig = liftM actionConfig getEnv

getExt :: (Extension e, Has e exts, Monad m) => proxy e -> ActionT exts m e
getExt p = liftM (getExtension p . actionExts) getEnv

getDocuments :: Monad m => ActionT exts m Documents
getDocuments = liftM actionDocuments getEnv

getRequestBody :: MonadIO m => ActionT exts m ([Param], [File])
getRequestBody = ActionT $ \e s c -> case actionReqBody s of
    Just b  -> c b s
    Nothing -> do
        (p,f) <- liftIO $ P.parseRequestBody P.lbsBackEnd (actionRequest e)
        let b = (p, map convFile f)
        c b s { actionReqBody = Just b }
  where
    convFile (p, P.FileInfo{..}) = File p fileName fileContentType fileContent

-- | parse request body and return params. since 0.9.0.0.
getReqParams :: MonadIO m => ActionT exts m [Param]
getReqParams = fst <$> getRequestBody

-- | parse request body and return files. since 0.9.0.0.
getReqFiles :: MonadIO m => ActionT exts m [File]
getReqFiles = snd <$> getRequestBody

--------------------------------------------------------------------------------

modifyState :: Monad m => (ActionState -> ActionState) -> ActionT exts m ()
modifyState f = ActionT $ \_ s c -> c () (f s)

getState :: ActionT exts m ActionState
getState = ActionT $ \_ s c -> c s s

-- | set status code. since 0.1.0.0.
status :: Monad m => Status -> ActionT exts m ()
status st = modifyState (\s -> s { actionStatus = st } )

-- | get all request headers. since 0.6.0.0.
getHeaders :: Monad m => ActionT exts m RequestHeaders
getHeaders = requestHeaders `liftM` getRequest

-- | modify response header. since 0.1.0.0.
modifyHeader :: Monad m => (ResponseHeaders -> ResponseHeaders) -> ActionT exts m ()
modifyHeader f = modifyState (\s -> s {actionHeaders = f $ actionHeaders s } )

-- | add response header. since 0.1.0.0.
addHeader :: Monad m => HeaderName -> S.ByteString -> ActionT exts m ()
addHeader h v = modifyHeader ((h,v):)

-- | set response headers. since 0.1.0.0.
setHeaders :: Monad m => ResponseHeaders -> ActionT exts m ()
setHeaders hs = modifyHeader (const hs)

type ContentType = S.ByteString

-- | set content-type header.
-- if content-type header already exists, replace it. since 0.1.0.0.
contentType :: Monad m => ContentType -> ActionT exts m ()
contentType c = modifyHeader
    (\h -> ("Content-Type", c) : filter (("Content-Type" /=) . fst) h)

--------------------------------------------------------------------------------

-- | stop handler and send current state. since 0.3.3.0.
stop :: Monad m => ActionT exts m a
stop = ActionT $ \_ s _ -> return $ Stop (toResponse s)

-- | stop with response. since 0.4.2.0.
stopWith :: Monad m => Response -> ActionT exts m a
stopWith a = ActionT $ \_ _ _ -> return $ Stop a

-- | redirect handler
--
-- set status and add location header. since 0.3.3.0.
--
-- rename from redirect in 0.6.2.0.
redirectWith :: Monad m
             => Status
             -> S.ByteString -- ^ Location redirect to
             -> ActionT exts m ()
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
redirectPermanently :: Monad m => S.ByteString -> ActionT exts m ()
redirectPermanently = redirectWith movedPermanently301

-- | redirect with:
--
-- 303 See Other (HTTP/1.1)  or
-- 302 Moved Temporarily (Other)
-- 
-- since 0.6.2.0.
redirect :: Monad m => S.ByteString -> ActionT exts m ()
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
redirectTemporary :: Monad m => S.ByteString -> ActionT exts m ()
redirectTemporary to = do
    v <- httpVersion <$> getRequest
    if v == http11
        then redirectWith temporaryRedirect307 to
        else redirectWith status302            to

-- | set raw response constructor. since 0.10.
--
-- example(use pipes-wai)
--
-- @
-- producer :: Monad m => Producer (Flush Builder) IO () -> ActionT exts m ()
-- producer = response (\s h -> responseProducer s h)
-- @
--
rawResponse :: Monad m => (Status -> ResponseHeaders -> Response) -> ActionT exts m ()
rawResponse f = modifyState (\s -> s { actionResponse = ResponseFunc f } )

-- | reset response body to no response. since v0.15.2.
reset :: Monad m => ActionT exts m ()
reset = modifyState (\s -> s { actionResponse = mempty } )

-- | set response body file content, without set Content-Type. since 0.1.0.0.
file' :: Monad m => FilePath -> Maybe FilePart -> ActionT exts m ()
file' f p = modifyState (\s -> s { actionResponse = ResponseFile f p } )

-- | set response body file content and detect Content-Type by extension. since 0.1.0.0.
file :: Monad m => FilePath -> Maybe FilePart -> ActionT exts m ()
file f p = do
    mime <- mimeType <$> getConfig
    contentType (mime f)
    file' f p

-- | append response body from builder. since 0.1.0.0.
builder :: Monad m => Builder -> ActionT exts m ()
builder b = modifyState (\s -> s { actionResponse = actionResponse s <> ResponseBuilder b } )

-- | append response body from strict bytestring. since 0.15.2.
bytes :: Monad m => S.ByteString -> ActionT exts m ()
bytes = builder . B.fromByteString

-- | append response body from lazy bytestring. since 0.15.2.
lazyBytes :: Monad m => L.ByteString -> ActionT exts m ()
lazyBytes = builder . B.fromLazyByteString

-- | append response body from strict text. encoding UTF-8. since 0.15.2.
text :: Monad m => T.Text -> ActionT exts m ()
text = builder . B.fromText

-- | append response body from lazy text. encoding UTF-8. since 0.15.2.
lazyText :: Monad m => TL.Text -> ActionT exts m ()
lazyText = builder . B.fromLazyText

-- | append response body from show. encoding UTF-8. since 0.15.2.
showing :: (Monad m, Show a) => a -> ActionT exts m ()
showing = builder . B.fromShow

-- | append response body from string. encoding UTF-8. since 0.15.2.
string :: Monad m => String -> ActionT exts m ()
string = builder . B.fromString

-- | append response body from char. encoding UTF-8. since 0.15.2.
char :: Monad m => Char -> ActionT exts m ()
char = builder . B.fromChar

-- | set response body source. since 0.9.0.0.
stream :: Monad m => StreamingBody -> ActionT exts m ()
stream str = modifyState (\s -> s { actionResponse = ResponseStream str })

{-# DEPRECATED source "use stream" #-}
source :: Monad m => StreamingBody -> ActionT exts m ()
source = stream

{-# DEPRECATED redirectFound, redirectSeeOther "use redirect" #-}
-- | redirect with 302 Found. since 0.3.3.0.
redirectFound       :: Monad m => S.ByteString -> ActionT exts m ()
redirectFound       = redirectWith found302

-- | redirect with 303 See Other. since 0.3.3.0.
redirectSeeOther    :: Monad m => S.ByteString -> ActionT exts m ()
redirectSeeOther    = redirectWith seeOther303

{-# DEPRECATED lbs "use lazyBytes" #-}
-- | append response body from lazy bytestring. since 0.1.0.0.
lbs :: Monad m => L.ByteString -> ActionT exts m ()
lbs = lazyBytes
