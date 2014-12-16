{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Apiary.Action.Internal where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote(QuasiQuoter(..))

import qualified System.PosixCompat.Files as Files

import Control.Applicative (Applicative(..), Alternative(..), (<$>))
import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.Trans(MonadIO(..), MonadTrans(..))
import Control.Monad.Base(MonadBase(..), liftBaseDefault)
import Control.Monad.Reader(MonadReader(..), ReaderT)
import Control.Monad.Catch(MonadThrow(..), MonadCatch(..), MonadMask(..))
import Control.Monad.Trans.Control
    (MonadTransControl(..), MonadBaseControl(..)
    , ComposeSt
    , defaultLiftBaseWith, defaultRestoreM)

import Network.Mime(defaultMimeLookup)
import Network.HTTP.Date(parseHTTPDate, epochTimeToHTTPDate, formatHTTPDate)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as P

import Data.Monoid(Monoid(..), (<>))
import qualified Data.Apiary.Dict as Dict
import Data.Apiary.Param(Param, File(..))
import Data.Apiary.Compat(SProxy(..))
import Data.Apiary.Document(Documents)
import Data.Apiary.Document.Html(defaultDocumentToHtml, DefaultDocumentConfig)
import Data.Default.Class(Default(..))

import Blaze.ByteString.Builder(Builder)
import Text.Blaze.Html.Renderer.Utf8(renderHtmlBuilder)
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vault.Lazy as V

data ApiaryConfig = ApiaryConfig
    { -- | call when no handler matched.
      notFound            :: Wai.Application
      -- | used unless call 'status' function.
    , defaultStatus       :: HTTP.Status
      -- | initial headers.
    , defaultHeaders      :: HTTP.ResponseHeaders
    , defaultContentType  :: S.ByteString
    , failStatus          :: HTTP.Status
    , failHeaders         :: HTTP.ResponseHeaders
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

defaultNotFound :: Wai.Application
defaultNotFound _ f = f      $ Wai.responseLBS HTTP.status404 [("Content-Type", "text/plain")] "404 Page Notfound.\n"

instance Default ApiaryConfig where
    def = ApiaryConfig 
        { notFound            = defaultNotFound
        , defaultStatus       = HTTP.ok200
        , defaultHeaders      = []
        , defaultContentType  = "text/plain"
        , failStatus          = HTTP.internalServerError500
        , failHeaders         = []
        , rootPattern         = ["index.html", "index.htm"]
        , mimeType            = defaultMimeLookup . T.pack
        }

--------------------------------------------------------------------------------

data ResponseBody
    = ResponseFile FilePath (Maybe Wai.FilePart)
    | ResponseBuilder Builder
    | ResponseStream Wai.StreamingBody
    | ResponseRaw (IO S.ByteString -> (S.ByteString -> IO ()) -> IO ()) Wai.Response
    | ResponseFunc (HTTP.Status -> HTTP.ResponseHeaders -> Wai.Response)

instance Monoid ResponseBody where
    mempty = ResponseBuilder mempty
    ResponseBuilder a `mappend` ResponseBuilder b = ResponseBuilder $ a <> b
    _ `mappend` b = b

toResponse :: ActionState -> Wai.Response
toResponse ActionState{..} = case actionResponse of
    ResponseFile  f p -> Wai.responseFile    actionStatus headers f p
    ResponseBuilder b -> Wai.responseBuilder actionStatus headers b
    ResponseStream  s -> Wai.responseStream  actionStatus headers s
    ResponseRaw   f r -> Wai.responseRaw f r
    ResponseFunc    f -> f actionStatus headers
  where
    headers = ("Content-Type", actionContentType) : actionHeaders

--------------------------------------------------------------------------------

data ActionState = ActionState
    { actionResponse    :: ResponseBody
    , actionStatus      :: HTTP.Status
    , actionHeaders     :: HTTP.ResponseHeaders
    , actionVault       :: V.Vault
    , actionContentType :: S.ByteString
    , actionReqBody     :: Maybe ([Param], [File])
    , actionFetches     :: [T.Text]
    }

initialState :: ApiaryConfig -> ActionState
initialState conf = ActionState
    { actionResponse    = ResponseBuilder mempty
    , actionStatus      = defaultStatus  conf
    , actionHeaders     = defaultHeaders conf
    , actionVault       = V.empty
    , actionContentType = defaultContentType conf
    , actionReqBody     = Nothing
    , actionFetches     = []
    }
{-# INLINE initialState #-}

--------------------------------------------------------------------------------
data Extensions (es :: [*]) where
    NoExtension  :: Extensions '[]
    AddExtension :: Extension e => (e :: *) -> Extensions es -> Extensions (e ': es)

type Middleware' = forall exts. ActionT exts '[] IO () -> ActionT exts '[] IO ()

class Extension e where
    extMiddleware :: e -> Wai.Middleware
    extMiddleware _ = id
    {-# INLINE extMiddleware #-}

    extMiddleware'  :: e -> Middleware'
    extMiddleware'  _ = id
    {-# INLINE extMiddleware' #-}

class Monad m => MonadExts es m | m -> es where
    getExts :: m (Extensions es)

instance Monad m => MonadExts es (ReaderT (Extensions es) m) where
    getExts = ask

--------------------------------------------------------------------------------

data ActionEnv exts = ActionEnv
    { actionConfig    :: ApiaryConfig
    , actionRequest   :: Wai.Request
    , actionDocuments :: Documents
    , actionExts      :: Extensions exts
    }

data Action a 
    = Continue ActionState a
    | Pass
    | Stop Wai.Response

newtype ActionT exts prms m a = ActionT { unActionT :: forall b. 
    Dict.Dict prms
    -> ActionEnv exts
    -> ActionState
    -> (a -> ActionState -> m (Action b))
    -> m (Action b)
    } deriving (Functor)

runActionT :: Monad m => ActionT exts prms m a
           -> Dict.Dict prms -> ActionEnv exts -> ActionState
           -> m (Action a)
runActionT m dict env st = unActionT m dict env st $ \a !st' ->
    return (Continue st' a)
{-# INLINE runActionT #-}

actionT :: Monad m 
        => (Dict.Dict prms -> ActionEnv exts -> ActionState -> m (Action a))
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

applyDict :: Dict.Dict prms -> ActionT exts prms m a -> ActionT exts '[] m a
applyDict d (ActionT m) = ActionT $ const (m d)
{-# INLINE applyDict #-}

execActionT :: ApiaryConfig -> Extensions exts -> Documents -> ActionT exts '[] IO () -> Wai.Application
execActionT config exts doc m request send = 
    runActionT m Dict.empty (ActionEnv config request doc exts) (initialState config) >>= \case
        Pass         -> notFound config request send
        Stop s       -> send s
        Continue r _ -> send $ toResponse r

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
        Stop (Wai.responseLBS (failStatus c) (failHeaders c) $ LC.pack s)

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

-- `MIN_VERSION_base(major1,major2,minor)
instance MonadTransControl (ActionT exts prms) where
#if MIN_VERSION_monad_control(1,0,0)
    type StT (ActionT exts prms) a = Action a
    liftWith f = actionT $ \prms e !s -> liftM (\a -> Continue s a) (f $ \t -> runActionT t prms e s)
    restoreT m = actionT $ \_ _ _ -> m
#else
    newtype StT (ActionT exts prms) a = StActionT { unStActionT :: Action a }
    liftWith f = actionT $ \prms e !s -> 
        liftM (\a -> Continue s a) (f $ \t -> liftM StActionT $ runActionT t prms e s)
    restoreT m = actionT $ \_ _ _ -> liftM unStActionT m
#endif

instance MonadBaseControl b m => MonadBaseControl b (ActionT exts prms m) where
#if MIN_VERSION_monad_control(1,0,0)
    type StM (ActionT exts prms m) a = ComposeSt (ActionT exts prms) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
#else
    newtype StM (ActionT exts prms m) a = StMActionT { unStMActionT :: ComposeSt (ActionT exts prms) m a }
    liftBaseWith = defaultLiftBaseWith StMActionT
    restoreM     = defaultRestoreM unStMActionT
#endif

instance MonadReader r m => MonadReader r (ActionT exts prms m) where
    ask     = lift ask
    local f = hoistActionT $ local f

instance Monad m => MonadExts exts (ActionT exts prms m) where
    getExts = liftM actionExts getEnv

--------------------------------------------------------------------------------

getEnv :: Monad m => ActionT exts prms m (ActionEnv exts)
getEnv = ActionT $ \_ e s c -> c e s
{-# INLINE getEnv #-}

-- | get raw request. since 0.1.0.0.
getRequest :: Monad m => ActionT exts prms m Wai.Request
getRequest = liftM actionRequest getEnv

getConfig :: Monad m => ActionT exts prms m ApiaryConfig
getConfig = liftM actionConfig getEnv

getParams :: Monad m => ActionT exts prms m (Dict.Dict prms)
getParams = ActionT $ \d _ s c -> c d s
{-# INLINE getParams #-}

-- | get parameter. since 1.0.0.
--
-- example:
--
-- > param [key|foo|]
--
param :: (Dict.Member k v prms, Monad m) => proxy k -> ActionT exts prms m v
param p = liftM (Dict.get p) getParams

paramsE :: [String] -> TH.ExpQ
paramsE ps = do
    ns <- mapM (\p -> (,) <$> TH.newName "x" <*> pure p) ps
    let bs  = map (\(v, k) -> TH.bindS (TH.varP v) (prm k)) ns
        tpl = TH.noBindS [| return $(TH.tupE $ map (TH.varE . fst) ns) |]
    TH.doE $ bs ++ [tpl]
  where
    prm  n = [| param (SProxy :: SProxy $(TH.litT $ TH.strTyLit n)) |]

-- | get parameters. since 1.0.0.
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

getQueryParams :: Monad m => ActionT exts prms m HTTP.Query
getQueryParams = Wai.queryString <$> getRequest

-- | parse request body and return params. since 1.0.0.
getReqBodyParams :: MonadIO m => ActionT exts prms m [Param]
getReqBodyParams = fst <$> getRequestBody

-- | parse request body and return files. since 0.9.0.0.
getReqBodyFiles :: MonadIO m => ActionT exts prms m [File]
getReqBodyFiles = snd <$> getRequestBody

-- | get all request headers. since 0.6.0.0.
getHeaders :: Monad m => ActionT exts prms m HTTP.RequestHeaders
getHeaders = Wai.requestHeaders `liftM` getRequest

--------------------------------------------------------------------------------

modifyState :: Monad m => (ActionState -> ActionState) -> ActionT exts prms m ()
modifyState f = ActionT $ \_ _ s c -> c () (f s)

-- | set status code. since 0.1.0.0.
status :: Monad m => HTTP.Status -> ActionT exts prms m ()
status st = modifyState (\s -> s { actionStatus = st } )

-- | modify response header. since 0.1.0.0.
--
-- Don't set Content-Type using this function. Use @contentType@.
modifyHeader :: Monad m => (HTTP.ResponseHeaders -> HTTP.ResponseHeaders) -> ActionT exts prms m ()
modifyHeader f = modifyState (\s -> s {actionHeaders = f $ actionHeaders s } )

-- | add response header. since 0.1.0.0.
--
-- Don't set Content-Type using this function. Use @contentType@.
addHeader :: Monad m => HTTP.HeaderName -> S.ByteString -> ActionT exts prms m ()
addHeader h v = modifyHeader ((h,v):)

-- | set response headers. since 0.1.0.0.
--
-- Don't set Content-Type using this function. Use @contentType@.
setHeaders :: Monad m => HTTP.ResponseHeaders -> ActionT exts prms m ()
setHeaders hs = modifyHeader (const hs)

type ContentType = S.ByteString

-- | set content-type header.
--
-- if content-type header already exists, replace it. since 0.1.0.0.
contentType :: Monad m => ContentType -> ActionT exts prms m ()
contentType c = modifyState (\s -> s { actionContentType = c } )

getState :: ActionT exts prms m ActionState
getState = ActionT $ \_ _ s c -> c s s

-- | lookup extensional state. since v1.2.0.
lookupVault :: V.Key a -> ActionT exts prms m (Maybe a)
lookupVault k = V.lookup k . actionVault <$> getState

-- | modify extensional state. since v1.2.0.
modifyVault :: (V.Vault -> V.Vault) -> ActionT exts prms m ()
modifyVault f = ActionT $ \_ _ s c -> c () (s {actionVault = f $ actionVault s})

-- | insert extensional state. since v1.2.0.
insertVault :: V.Key a -> a -> ActionT exts prms m ()
insertVault k i = modifyVault $ V.insert k i

-- | adjust extensional state. since v1.2.0.
adjustVault :: (a -> a) -> V.Key a -> ActionT exts prms m ()
adjustVault f k = modifyVault $ V.adjust f k

-- | delete extensional state. since v1.2.0.
deleteVault :: V.Key a -> ActionT exts prms m ()
deleteVault k = modifyVault $ V.delete k

--------------------------------------------------------------------------------

-- | stop handler and send current state. since 0.3.3.0.
stop :: Monad m => ActionT exts prms m a
stop = ActionT $ \_ _ s _ -> return $ Stop (toResponse s)

-- | stop with response. since 0.4.2.0.
stopWith :: Monad m => Wai.Response -> ActionT exts prms m a
stopWith a = ActionT $ \_ _ _ _ -> return $ Stop a

-- | redirect handler
--
-- set status and add location header. since 0.3.3.0.
--
-- rename from redirect in 0.6.2.0.
redirectWith :: Monad m
             => HTTP.Status
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
redirectPermanently = redirectWith HTTP.movedPermanently301

-- | redirect with:
--
-- 303 See Other (HTTP/1.1)  or
-- 302 Moved Temporarily (Other)
-- 
-- since 0.6.2.0.
redirect :: Monad m => S.ByteString -> ActionT exts prms m ()
redirect to = do
    v <- Wai.httpVersion <$> getRequest
    if v == HTTP.http11
        then redirectWith HTTP.seeOther303 to
        else redirectWith HTTP.status302   to

-- | redirect with:
--
-- 307 Temporary Redirect (HTTP/1.1) or
-- 302 Moved Temporarily (Other)
--
-- since 0.3.3.0.
redirectTemporary :: Monad m => S.ByteString -> ActionT exts prms m ()
redirectTemporary to = do
    v <- Wai.httpVersion <$> getRequest
    if v == HTTP.http11
        then redirectWith HTTP.temporaryRedirect307 to
        else redirectWith HTTP.status302            to

-- | set raw response constructor. since 0.10.
--
-- example(use pipes-wai)
--
-- @
-- producer :: Monad m => Producer (Flush Builder) IO () -> ActionT' exts m ()
-- producer = response (\s h -> responseProducer s h)
-- @
--
rawResponse :: Monad m => (HTTP.Status -> HTTP.ResponseHeaders -> Wai.Response) -> ActionT exts prms m ()
rawResponse f = modifyState (\s -> s { actionResponse = ResponseFunc f } )

-- | reset response body to no response. since v0.15.2.
reset :: Monad m => ActionT exts prms m ()
reset = modifyState (\s -> s { actionResponse = mempty } )

-- | set response body file content, without set Content-Type. since 0.1.0.0.
file' :: MonadIO m => FilePath -> Maybe Wai.FilePart -> ActionT exts prms m ()
file' f p = modifyState (\s -> s { actionResponse = ResponseFile f p } )

-- | set response body file content and detect Content-Type by extension. since 0.1.0.0.
--
-- file modification check since 0.17.2.
file :: MonadIO m => FilePath -> Maybe Wai.FilePart -> ActionT exts prms m ()
file f p = do
    mbims <- (>>= parseHTTPDate) . lookup "If-Modified-Since" <$> getHeaders
    e <- liftIO $ Files.fileExist f
    t <- if e
         then liftIO $ Just . epochTimeToHTTPDate . Files.modificationTime <$> Files.getFileStatus f
         else return Nothing
    case mbims of
        Just ims | maybe False (ims >=) t -> reset >> status HTTP.status304 >> stop
        _ -> do
            mime <- mimeType <$> getConfig
            contentType (mime f)
            maybe (return ()) (addHeader "Last-Modified" . formatHTTPDate) t
            file' f p

devFile' :: MonadIO m => FilePath -> ActionT exts prms m ()
devFile' f = liftIO (Files.fileExist f) >>= \e ->
    if e
    then liftIO (L.readFile f) >>= lazyBytes
    else mzero

-- | send file contents as lazy bytestring response. since v1.1.4.
devFile :: MonadIO m => FilePath -> ActionT exts prms m ()
devFile f = do
    mime <- mimeType <$> getConfig
    contentType (mime f)
    devFile' f

-- | set response body from builder. since 0.1.0.0.
builder :: Monad m => Builder -> ActionT exts prms m ()
builder b = modifyState (\s -> s { actionResponse = ResponseBuilder b } )

-- | set response body from strict bytestring. since 0.15.2.
bytes :: Monad m => S.ByteString -> ActionT exts prms m ()
bytes = builder . B.fromByteString

-- | set response body from lazy bytestring. since 0.15.2.
lazyBytes :: Monad m => L.ByteString -> ActionT exts prms m ()
lazyBytes = builder . B.fromLazyByteString

-- | set response body from strict text. encoding UTF-8. since 0.15.2.
text :: Monad m => T.Text -> ActionT exts prms m ()
text = builder . B.fromText

-- | set response body from lazy text. encoding UTF-8. since 0.15.2.
lazyText :: Monad m => TL.Text -> ActionT exts prms m ()
lazyText = builder . B.fromLazyText

-- | set response body from show. encoding UTF-8. since 0.15.2.
showing :: (Monad m, Show a) => a -> ActionT exts prms m ()
showing = builder . B.fromShow

-- | set response body from string. encoding UTF-8. since 0.15.2.
string :: Monad m => String -> ActionT exts prms m ()
string = builder . B.fromString

-- | set response body from char. encoding UTF-8. since 0.15.2.
char :: Monad m => Char -> ActionT exts prms m ()
char = builder . B.fromChar

-- | append response body from builder. since 1.2.0.
appendBuilder :: Monad m => Builder -> ActionT exts prms m ()
appendBuilder b = modifyState (\s -> s { actionResponse = actionResponse s <> ResponseBuilder b } )

-- | append response body from strict bytestring. since 1.2.0.
appendBytes :: Monad m => S.ByteString -> ActionT exts prms m ()
appendBytes = appendBuilder . B.fromByteString

-- | append response body from lazy bytestring. since 1.2.0.
appendLazyBytes :: Monad m => L.ByteString -> ActionT exts prms m ()
appendLazyBytes = appendBuilder . B.fromLazyByteString

-- | append response body from strict text. encoding UTF-8. since 1.2.0.
appendText :: Monad m => T.Text -> ActionT exts prms m ()
appendText = appendBuilder . B.fromText

-- | append response body from lazy text. encoding UTF-8. since 1.2.0.
appendLazyText :: Monad m => TL.Text -> ActionT exts prms m ()
appendLazyText = appendBuilder . B.fromLazyText

-- | append response body from show. encoding UTF-8. since 1.2.0.
appendShowing :: (Monad m, Show a) => a -> ActionT exts prms m ()
appendShowing = appendBuilder . B.fromShow

-- | append response body from string. encoding UTF-8. since 1.2.0.
appendString :: Monad m => String -> ActionT exts prms m ()
appendString = appendBuilder . B.fromString

-- | append response body from char. encoding UTF-8. since 1.2.0.
appendChar :: Monad m => Char -> ActionT exts prms m ()
appendChar = appendBuilder . B.fromChar

-- | set response body source. since 0.9.0.0.
stream :: Monad m => Wai.StreamingBody -> ActionT exts prms m ()
stream str = modifyState (\s -> s { actionResponse = ResponseStream str })
