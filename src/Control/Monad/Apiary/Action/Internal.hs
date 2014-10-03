{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Apiary.Action.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import System.PosixCompat.Files

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans.Control

import Network.Mime hiding (Extension)
import Network.HTTP.Date
import Network.HTTP.Types as Http
import Network.Wai
import qualified Network.Wai.Parse as P

import Data.Monoid hiding (All)
import Data.Apiary.Extension
import Data.Apiary.Dict
import Data.Apiary.Param
import Data.Apiary.Compat
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

-- | auto generated document.
defaultDocumentationAction :: Monad m => DefaultDocumentConfig -> ActionT exts prms m ()
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

newtype ActionT exts prms m a = ActionT { unActionT :: forall b. 
    Dict prms
    -> ActionEnv exts
    -> ActionState
    -> (a -> ActionState -> m (Action b))
    -> m (Action b)
    } deriving (Functor)

runActionT :: Monad m => ActionT exts prms m a
           -> Dict prms -> ActionEnv exts -> ActionState
           -> m (Action a)
runActionT m dict env st = unActionT m dict env st $ \a !st' ->
    return (Continue st' a)
{-# INLINE runActionT #-}

actionT :: Monad m 
        => (Dict prms -> ActionEnv exts -> ActionState -> m (Action a))
        -> ActionT exts prms m a
actionT f = ActionT $ \dict env !st cont -> f dict env st >>= \case
    Pass            -> return Pass
    Stop s          -> return $ Stop s
    Continue !st' a -> cont a st'
{-# INLINE actionT #-}

-- | n must be Monad, so cant be MFunctor.
hoistActionT :: (Monad m, Monad n)
             => (forall b. m b -> n b) -> ActionT exts prms m a -> ActionT exts prms n a
hoistActionT run m = actionT $ \d e s -> run (runActionT m d e s)
{-# INLINE hoistActionT #-}

newtype ActionT' exts m a = ActionT' { unActionT' :: forall b. 
    ActionEnv exts
    -> ActionState
    -> (a -> ActionState -> m (Action b))
    -> m (Action b)
    } deriving (Functor)

applyDict :: Dict prms -> ActionT exts prms m a -> ActionT' exts m a
applyDict d (ActionT m) = ActionT' (m d)
{-# INLINE applyDict #-}

actionT' :: Monad m 
         => (ActionEnv exts -> ActionState -> m (Action a))
         -> ActionT' exts m a
actionT' f = ActionT' $ \env !st cont -> f env st >>= \case
    Pass            -> return Pass
    Stop s          -> return $ Stop s
    Continue !st' a -> cont a st'
{-# INLINE actionT' #-}

runActionT' :: Monad m => ActionT' exts m a
            -> ActionEnv exts -> ActionState
            -> m (Action a)
runActionT' m env st = unActionT' m env st $ \a !st' -> return (Continue st' a)
{-# INLINE runActionT' #-}

hoistActionT' :: (Monad m, Monad n)
              => (forall b. m b -> n b) -> ActionT' exts m a -> ActionT' exts n a
hoistActionT' run m = actionT' $ \e s -> run (runActionT' m e s)
{-# INLINE hoistActionT' #-}

execActionT' :: ApiaryConfig -> Extensions exts -> Documents -> ActionT' exts IO () -> Application
#ifdef WAI3
execActionT' config exts doc m request send = 
#else
execActionT' config exts doc m request = let send = return in
#endif
    runActionT' m (ActionEnv config request doc exts) (initialState config) >>= \case
#ifdef WAI3
        Pass         -> notFound config request send
#else
        Pass         -> notFound config request
#endif
        Stop s       -> send s
        Continue r _ -> send $ toResponse r

instance Applicative (ActionT' exts m) where
    pure x = ActionT' $ \_ !st cont -> cont x st
    mf <*> ma = ActionT' $ \env st cont ->
        unActionT' mf env st  $ \f !st'  ->
        unActionT' ma env st' $ \a !st'' ->
        cont (f a) st''

instance Monad m => Monad (ActionT' exts m) where
    return x = ActionT' $ \_ !st cont -> cont x st
    m >>= k  = ActionT' $ \env !st cont ->
        unActionT' m env st $ \a !st' ->
        unActionT' (k a) env st' cont
    fail s = ActionT' $ \(ActionEnv{actionConfig = c}) _ _ -> return $
        Stop (responseLBS (failStatus c) (failHeaders c) $ LC.pack s)

instance (Monad m, Functor m) => Alternative (ActionT' exts m) where
    empty = mzero
    (<|>) = mplus
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance Monad m => MonadPlus (ActionT' exts m) where
    mzero = ActionT' $ \_ _ _ -> return Pass
    mplus m n = ActionT' $ \e !s cont -> unActionT' m e s cont >>= \case
        Continue !st a -> return $ Continue st a
        Stop stp       -> return $ Stop stp
        Pass           -> unActionT' n e s cont
    {-# INLINE mzero #-}
    {-# INLINE mplus #-}
--------------------------------------------------------------------------------

instance Applicative (ActionT exts prms m) where
    pure x = ActionT $ \_ _ !st cont -> cont x st
    mf <*> ma = ActionT $ \dict env st cont ->
        unActionT mf dict env st  $ \f !st'  ->
        unActionT ma dict env st' $ \a !st'' ->
        cont (f a) st''

instance Monad m => Monad (ActionT exts prms m) where
    return x = ActionT $ \_ _ !st cont -> cont x st
    m >>= k  = ActionT $ \dict env !st cont ->
        unActionT m dict env st $ \a !st' ->
        unActionT (k a) dict env st' cont
    fail s = ActionT $ \_ (ActionEnv{actionConfig = c}) _ _ -> return $
        Stop (responseLBS (failStatus c) (failHeaders c) $ LC.pack s)

instance MonadIO m => MonadIO (ActionT exts prms m) where
    liftIO m = ActionT $ \_ _ !st cont ->
        liftIO m >>= \a -> cont a st

instance MonadTrans (ActionT exts prms) where
    lift m = ActionT $ \_ _ !st cont ->
        m >>= \a -> cont a st

instance MonadThrow m => MonadThrow (ActionT exts prms m) where
    throwM e = ActionT $ \_ _ !st cont ->
        throwM e >>= \a -> cont a st

instance MonadCatch m => MonadCatch (ActionT exts prms m) where
    catch m h = ActionT $ \dict env !st cont ->
        catch (unActionT m dict env st cont) (\e -> unActionT (h e) dict env st cont)
    {-# INLINE catch #-}

instance MonadMask m => MonadMask (ActionT exts prms m) where
    mask a = ActionT $ \dict env !st cont ->
        mask $ \u -> unActionT (a $ q u) dict env st cont
      where
        q u m = actionT $ \dict env !st -> u (runActionT m dict env st)
    uninterruptibleMask a = ActionT $ \dict env !st cont ->
        uninterruptibleMask $ \u -> unActionT (a $ q u) dict env st cont
      where
        q u m = actionT $ \dict env !st -> u (runActionT m dict env st)
    {-# INLINE mask #-}
    {-# INLINE uninterruptibleMask #-}

instance (Monad m, Functor m) => Alternative (ActionT exts prms m) where
    empty = mzero
    (<|>) = mplus
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance Monad m => MonadPlus (ActionT exts prms m) where
    mzero = ActionT $ \_ _ _ _ -> return Pass
    mplus m n = ActionT $ \dict e !s cont -> unActionT m dict e s cont >>= \case
        Continue !st a -> return $ Continue st a
        Stop stp       -> return $ Stop stp
        Pass           -> unActionT n dict e s cont
    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

instance MonadBase b m => MonadBase b (ActionT exts prms m) where
    liftBase = liftBaseDefault

instance MonadTransControl (ActionT exts prms) where
    newtype StT (ActionT exts prms) a = StActionT { unStActionT :: Action a }
    liftWith f = actionT $ \prms e !s -> 
        liftM (\a -> Continue s a) (f $ \t -> liftM StActionT $ runActionT t prms e s)
    restoreT m = actionT $ \_ _ _ -> liftM unStActionT m

instance MonadBaseControl b m => MonadBaseControl b (ActionT exts prms m) where
    newtype StM (ActionT exts prms m) a = StMActionT { unStMActionT :: ComposeSt (ActionT exts prms) m a }
    liftBaseWith = defaultLiftBaseWith StMActionT
    restoreM     = defaultRestoreM unStMActionT

instance MonadReader r m => MonadReader r (ActionT exts prms m) where
    ask     = lift ask
    local f = hoistActionT $ local f

--------------------------------------------------------------------------------

getEnv :: Monad m => ActionT exts prms m (ActionEnv exts)
getEnv = ActionT $ \_ e s c -> c e s

getRequest' :: Monad m => ActionT' exts m Request
getRequest' = ActionT' $ \e s c -> c (actionRequest e) s

-- | get raw request. since 0.1.0.0.
getRequest :: Monad m => ActionT exts prms m Request
getRequest = liftM actionRequest getEnv

getConfig :: Monad m => ActionT exts prms m ApiaryConfig
getConfig = liftM actionConfig getEnv

-- | get extension.
getExt :: (Has e exts, Monad m) => proxy e -> ActionT exts prms m e
getExt p = liftM (getExtension p . actionExts) getEnv

getParams :: Monad m => ActionT exts prms m (Dict prms)
getParams = ActionT $ \d _ s c -> c d s

-- | get parameter. since 0.18.0.
--
-- example:
--
-- > param [key|foo|]
--
param :: (Member k v prms, Monad m) => proxy k -> ActionT exts prms m v
param p = liftM (get p) getParams

paramsE :: [String] -> ExpQ
paramsE ps = do
    ns <- mapM (\p -> (,) <$> newName "x" <*> pure p) ps
    let bs  = map (\(v, k) -> bindS (varP v) (prm k)) ns
        tpl = noBindS [| return $(tupE $ map (varE . fst) ns) |]
    doE $ bs ++ [tpl]
  where
    prm  n = [| param (SProxy :: SProxy $(litT $ strTyLit n)) |]

-- | get parameters. since 0.18.0.
--
-- > [params|foo,bar|] == do { a <- param [key|foo|]; b <- param [key|bar|]; return (a, b) }
--
params :: QuasiQuoter
params = QuasiQuoter
    { quoteExp  = paramsE . map (T.unpack . T.strip) . T.splitOn "," . T.pack
    , quotePat  = error "params QQ is defined only exp."
    , quoteType = error "params QQ is defined only exp."
    , quoteDec  = error "params QQ is defined only exp."
    }

getDocuments :: Monad m => ActionT exts prms m Documents
getDocuments = liftM actionDocuments getEnv

getRequestBody :: MonadIO m => ActionT exts prms m ([Param], [File])
getRequestBody = ActionT $ \_ e s c -> case actionReqBody s of
    Just b  -> c b s
    Nothing -> do
        (p,f) <- liftIO $ P.parseRequestBody P.lbsBackEnd (actionRequest e)
        let b = (p, map convFile f)
        c b s { actionReqBody = Just b }
  where
    convFile (p, P.FileInfo{..}) = File p fileName fileContentType fileContent

getQueryParams :: Monad m => ActionT exts prms m Http.Query
getQueryParams = queryString <$> getRequest

-- | parse request body and return params. since 0.18.0.0.
getReqBodyParams :: MonadIO m => ActionT exts prms m [Param]
getReqBodyParams = fst <$> getRequestBody

-- | parse request body and return files. since 0.9.0.0.
getReqBodyFiles :: MonadIO m => ActionT exts prms m [File]
getReqBodyFiles = snd <$> getRequestBody

--------------------------------------------------------------------------------

modifyState' :: Monad m => (ActionState -> ActionState) -> ActionT' exts m ()
modifyState' f = ActionT' $ \_ s c -> c () (f s)

modifyState :: Monad m => (ActionState -> ActionState) -> ActionT exts prms m ()
modifyState f = ActionT $ \_ _ s c -> c () (f s)

getState :: ActionT exts prms m ActionState
getState = ActionT $ \_ _ s c -> c s s

-- | set status code. since 0.1.0.0.
status :: Monad m => Status -> ActionT exts prms m ()
status st = modifyState (\s -> s { actionStatus = st } )

-- | get all request headers. since 0.6.0.0.
getHeaders :: Monad m => ActionT exts prms m RequestHeaders
getHeaders = requestHeaders `liftM` getRequest

-- | modify response header. since 0.1.0.0.
modifyHeader :: Monad m => (ResponseHeaders -> ResponseHeaders) -> ActionT exts prms m ()
modifyHeader f = modifyState (\s -> s {actionHeaders = f $ actionHeaders s } )

-- | add response header. since 0.1.0.0.
addHeader :: Monad m => HeaderName -> S.ByteString -> ActionT exts prms m ()
addHeader h v = modifyHeader ((h,v):)

-- | set response headers. since 0.1.0.0.
setHeaders :: Monad m => ResponseHeaders -> ActionT exts prms m ()
setHeaders hs = modifyHeader (const hs)

type ContentType = S.ByteString

-- | set content-type header.
-- if content-type header already exists, replace it. since 0.1.0.0.
contentType :: Monad m => ContentType -> ActionT exts prms m ()
contentType c = modifyHeader
    (\h -> ("Content-Type", c) : filter (("Content-Type" /=) . fst) h)

--------------------------------------------------------------------------------

-- | stop handler and send current state. since 0.3.3.0.
stop :: Monad m => ActionT exts prms m a
stop = ActionT $ \_ _ s _ -> return $ Stop (toResponse s)

-- | stop with response. since 0.4.2.0.
stopWith :: Monad m => Response -> ActionT exts prms m a
stopWith a = ActionT $ \_ _ _ _ -> return $ Stop a

-- | redirect handler
--
-- set status and add location header. since 0.3.3.0.
--
-- rename from redirect in 0.6.2.0.
redirectWith :: Monad m
             => Status
             -> S.ByteString -- ^ Location redirect to
             -> ActionT exts prms m ()
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
redirectPermanently :: Monad m => S.ByteString -> ActionT exts prms m ()
redirectPermanently = redirectWith movedPermanently301

-- | redirect with:
--
-- 303 See Other (HTTP/1.1)  or
-- 302 Moved Temporarily (Other)
-- 
-- since 0.6.2.0.
redirect :: Monad m => S.ByteString -> ActionT exts prms m ()
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
redirectTemporary :: Monad m => S.ByteString -> ActionT exts prms m ()
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
-- producer :: Monad m => Producer (Flush Builder) IO () -> ActionT' exts m ()
-- producer = response (\s h -> responseProducer s h)
-- @
--
rawResponse :: Monad m => (Status -> ResponseHeaders -> Response) -> ActionT exts prms m ()
rawResponse f = modifyState (\s -> s { actionResponse = ResponseFunc f } )

-- | reset response body to no response. since v0.15.2.
reset :: Monad m => ActionT exts prms m ()
reset = modifyState (\s -> s { actionResponse = mempty } )

-- | set response body file content, without set Content-Type. since 0.1.0.0.
file' :: MonadIO m => FilePath -> Maybe FilePart -> ActionT exts prms m ()
file' f p = modifyState (\s -> s { actionResponse = ResponseFile f p } )

-- | set response body file content and detect Content-Type by extension. since 0.1.0.0.
--
-- file modification check since 0.17.2.
file :: MonadIO m => FilePath -> Maybe FilePart -> ActionT exts prms m ()
file f p = do
    mbims <- (>>= parseHTTPDate) . lookup "If-Modified-Since" <$> getHeaders
    e <- liftIO $ fileExist f
    t <- if e
         then liftIO $ Just . epochTimeToHTTPDate . modificationTime <$> getFileStatus f
         else return Nothing
    case mbims of
        Just ims | maybe False (ims >=) t -> reset >> status status304 >> stop
        _ -> do
            mime <- mimeType <$> getConfig
            contentType (mime f)
            maybe (return ()) (addHeader "Last-Modified" . formatHTTPDate) t
            file' f p

-- | append response body from builder. since 0.1.0.0.
builder :: Monad m => Builder -> ActionT exts prms m ()
builder b = modifyState (\s -> s { actionResponse = actionResponse s <> ResponseBuilder b } )

-- | append response body from strict bytestring. since 0.15.2.
bytes :: Monad m => S.ByteString -> ActionT exts prms m ()
bytes = builder . B.fromByteString

-- | append response body from lazy bytestring. since 0.15.2.
lazyBytes :: Monad m => L.ByteString -> ActionT exts prms m ()
lazyBytes = builder . B.fromLazyByteString

-- | append response body from strict text. encoding UTF-8. since 0.15.2.
text :: Monad m => T.Text -> ActionT exts prms m ()
text = builder . B.fromText

-- | append response body from lazy text. encoding UTF-8. since 0.15.2.
lazyText :: Monad m => TL.Text -> ActionT exts prms m ()
lazyText = builder . B.fromLazyText

-- | append response body from show. encoding UTF-8. since 0.15.2.
showing :: (Monad m, Show a) => a -> ActionT exts prms m ()
showing = builder . B.fromShow

-- | append response body from string. encoding UTF-8. since 0.15.2.
string :: Monad m => String -> ActionT exts prms m ()
string = builder . B.fromString

-- | append response body from char. encoding UTF-8. since 0.15.2.
char :: Monad m => Char -> ActionT exts prms m ()
char = builder . B.fromChar

-- | set response body source. since 0.9.0.0.
stream :: Monad m => StreamingBody -> ActionT exts prms m ()
stream str = modifyState (\s -> s { actionResponse = ResponseStream str })
