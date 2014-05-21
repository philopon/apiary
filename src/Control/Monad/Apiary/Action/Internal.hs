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
import Control.Monad.Trans.Control
import Network.Wai
import Network.Mime
import Data.Default.Class
import Data.Monoid
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

data Act a 
    = Continue a
    | Pass
    | Stop Response

newtype Action a = Action { unAction :: forall b. 
    ApiaryConfig
    -> Request
    -> ActionState
    -> (a -> ActionState -> IO (Act b))
    -> IO (Act b)
    }

instance Functor Action where
    fmap f m = Action $ \conf req st cont ->
        unAction m conf req st (\a s' -> s' `seq` cont (f a) s')

instance Applicative Action where
    pure x = Action $ \_ _ st cont -> cont x st
    mf <*> ma = Action $ \conf req st cont ->
        unAction mf conf req st  $ \f st'  ->
        unAction ma conf req st' $ \a st'' ->
        st' `seq` st'' `seq` cont (f a) st''

instance Monad Action where
    return x = Action $ \_ _ st cont -> cont x st
    m >>= k  = Action $ \conf req st cont ->
        unAction m conf req st $ \a st' ->
        st' `seq` unAction (k a) conf req st' cont
    fail _ = Action $ \_ _ _ _ -> return Pass

instance MonadIO Action where
    liftIO m = Action $ \_ _ st cont ->
        liftIO m >>= \a -> cont a st

runAction :: Action a
           -> ApiaryConfig -> Request -> ActionState
           -> IO (Act (a, ActionState))
runAction m conf req st = unAction m conf req st $ \a st' ->
    st' `seq` return (Continue (a, st'))

action :: (ApiaryConfig -> Request -> ActionState -> IO (Act (a, ActionState)))
        -> Action a
action f = Action $ \conf req st cont -> f conf req st >>= \case
    Pass             -> return Pass
    Stop s           -> return $ Stop s
    Continue (a,st') -> st' `seq` cont a st'

execAction :: ApiaryConfig -> Action () -> Application
execAction config m request = runAction m config request resp >>= \case
        Pass           -> notFound config request
        Stop s         -> return s
        Continue (_,r) -> return $ actionStateToResponse r
  where
    resp = ActionState (defaultStatus config) (defaultHeader config) (LBS "")

instance Alternative Action where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Action where
    mzero = action $ \_ _ _ -> return Pass
    mplus m n = action $ \c r s -> runAction m c r s >>= \case
        Continue a -> return $ Continue a
        Stop stp   -> return $ Stop stp
        Pass       -> runAction n c r s

instance Monoid (Action ()) where
    mempty  = mzero
    mappend = mplus

instance MonadBase IO Action where
    liftBase = liftIO

instance MonadBaseControl IO Action where
    newtype StM Action a = StMAction { unStMAction :: Act (a, ActionState) }
    liftBaseWith f = action $ \c r s ->
        liftM (\a -> Continue (a, s)) (f $ \t -> liftM StMAction $ runAction t c r s)
    restoreM m = action $ \_ _ _ -> return (unStMAction m)

-- | stop handler and send current state. since 0.3.3.0.
stop :: Action a
stop = Action $ \_ _ s _ -> return $ Stop (actionStateToResponse s)

-- | stop with response. since 0.4.2.0.
stopWith :: Response -> Action a
stopWith a = Action $ \_ _ _ _ -> return $ Stop a

-- | get raw request. since 0.1.0.0.
getRequest :: Action Request
getRequest = Action $ \_ r s c -> c r s

getConfig :: Action ApiaryConfig
getConfig = Action $ \c _ s cont -> cont c s

modifyState :: (ActionState -> ActionState) -> Action ()
modifyState f = Action $ \_ _ s c -> c () (f s)

-- | get all request headers. since 0.6.0.0.
getHeaders :: Action RequestHeaders
getHeaders = requestHeaders `liftM` getRequest

-- | set status code. since 0.1.0.0.
status :: Status -> Action ()
status st = modifyState (\s -> s { actionStatus = st } )

-- | modify response header. since 0.1.0.0.
modifyHeader :: (ResponseHeaders -> ResponseHeaders) -> Action ()
modifyHeader f = modifyState (\s -> s {actionHeaders = f $ actionHeaders s } )

-- | add response header. since 0.1.0.0.
addHeader :: HeaderName -> S.ByteString -> Action ()
addHeader h v = modifyHeader ((h,v):)

-- | set response headers. since 0.1.0.0.
setHeaders :: ResponseHeaders -> Action ()
setHeaders hs = modifyHeader (const hs)

-- | set content-type header.
-- if content-type header already exists, replace it. since 0.1.0.0.
contentType :: S.ByteString -> Action ()
contentType c = modifyHeader
    (\h -> ("Content-Type", c) : filter (("Content-Type" /=) . fst) h)

-- | redirect handler
--
-- set status, add location header. since 0.3.3.0.
--
-- rename from redirect in 0.6.2.0.
redirectWith :: Status
             -> S.ByteString -- ^ Location redirect to
             -> Action ()
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
redirectPermanently :: S.ByteString -> Action ()
redirectPermanently = redirectWith movedPermanently301

-- | redirect with:
--
-- 303 See Other (HTTP/1.1)  or
-- 302 Moved Temporarily (Other)
-- 
-- since 0.6.2.0.
redirect :: S.ByteString -> Action ()
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
redirectTemporary :: S.ByteString -> Action ()
redirectTemporary to = do
    v <- httpVersion <$> getRequest
    if v == http11
        then redirectWith temporaryRedirect307 to
        else redirectWith status302            to

-- | set response body file content and detect Content-Type by extension. since 0.1.0.0.
file :: FilePath -> Maybe FilePart -> Action ()
file f p = do
    mime <- mimeType <$> getConfig
    contentType (mime f)
    file' f p

-- | set response body file content, without set Content-Type. since 0.1.0.0.
file' :: FilePath -> Maybe FilePart -> Action ()
file' f p = modifyState (\s -> s { actionBody = File f p } )

-- | set response body builder. since 0.1.0.0.
builder :: Builder -> Action ()
builder b = modifyState (\s -> s { actionBody = Builder b } )

-- | set response body lazy bytestring. since 0.1.0.0.
lbs :: L.ByteString -> Action ()
lbs l = modifyState (\s -> s { actionBody = LBS l } )

-- | set response body source. since 0.1.0.0.
source :: Source IO (Flush Builder) -> Action ()
source src = modifyState (\s -> s { actionBody = SRC src } )

{-# DEPRECATED redirectFound, redirectSeeOther "use redirect" #-}
-- | redirect with 302 Found. since 0.3.3.0.
redirectFound       :: S.ByteString -> Action ()
redirectFound       = redirectWith found302

-- | redirect with 303 See Other. since 0.3.3.0.
redirectSeeOther    :: S.ByteString -> Action ()
redirectSeeOther    = redirectWith seeOther303
