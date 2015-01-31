{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Apiary.Internal
    ( ApiaryT

    , runApiaryTWith
    , runApiaryWith
    , runApiary
    , ApiaryConfig(..)

    , action

    , middleware
    , group
    , document
    , precondition
    , noDoc

    , apiaryConfig
    , apiaryExt

    -- internal
    , Filter
    , Filter'
    , focus
    ) where

import qualified Network.Wai as Wai

import Control.Applicative(Applicative(..), (<$>))
import Control.Monad(liftM)
import Control.Monad.Trans(MonadIO(liftIO), MonadTrans(lift))
import Control.Monad.Trans.Control
    ( MonadTransControl(..), MonadBaseControl(..)
    , ComposeSt, defaultLiftBaseWith, defaultRestoreM
    )
import Control.Monad.Base(MonadBase(..))
import Control.Monad.Apiary.Action.Internal
    (ActionT, ApiaryConfig, getRequest
    , Extensions(NoExtension)
    , execActionT, hoistActionT, applyDict, rootPattern
    )

import qualified Network.Routing as R
import Data.Apiary.Method(Method, renderMethod)
import Data.Apiary.Extension ( Has, MonadExts(..), getExt, noExtension )
import Data.Apiary.Extension.Internal(Initializer(..), allMiddleware, allMiddleware')
import Data.Apiary.Document.Internal(Doc(..), docsToDocuments)
import Data.Monoid(Monoid(..), (<>))

import Text.Blaze.Html(Html)
import qualified Data.Text as T

-- | routing filter
type Filter exts actM m inp out =
    ApiaryT exts out actM m () -> ApiaryT exts inp actM m ()

-- | routing filter(without modify parameter dictionary)
type Filter' exts actM m = forall prms. Filter exts actM m prms prms

type ActionT' exts actM = ActionT exts '[] actM

data ApiaryEnv exts prms actM = ApiaryEnv
    { envMethod :: Maybe Method
    , envPath   :: R.Path prms (ActionT' exts actM) () -> R.Path '[] (ActionT' exts actM) ()
    , envConfig :: ApiaryConfig
    , envDoc    :: Doc -> Doc
    , envExts   :: Extensions exts
    }

initialEnv :: Monad actM => ApiaryConfig -> Extensions exts -> ApiaryEnv exts '[] actM
initialEnv conf = ApiaryEnv Nothing id conf id

data ApiaryWriter exts actM = ApiaryWriter
    { writerRouter :: R.Router '[] (ActionT' exts actM) () -> R.Router '[] (ActionT' exts actM) ()
    , writerDoc    :: [Doc] -> [Doc]
    , writerMw     :: Wai.Middleware
    }

instance Monoid (ApiaryWriter exts actM) where
    mempty = ApiaryWriter id id id
    ApiaryWriter ra da am `mappend` ApiaryWriter rb db bm
        = ApiaryWriter (ra . rb) (da . db) (am . bm)

-- | Apiary monad. since 0.8.0.0.
newtype ApiaryT exts prms actM m a = ApiaryT { unApiaryT :: forall b.
    ApiaryEnv exts prms actM
    -> (a -> ApiaryWriter exts actM -> m b)
    -> m b 
    }

apiaryT :: Monad m
        => (ApiaryEnv exts prms actM -> m (a, ApiaryWriter exts actM))
        -> ApiaryT exts prms actM m a
apiaryT f = ApiaryT $ \rdr cont -> f rdr >>= \(a, w) -> cont a w

routerToAction :: Monad actM => R.Router '[] (ActionT' exts actM) () -> ActionT' exts actM ()
routerToAction router = do
    req <- getRequest
    R.execute router (Wai.requestMethod req) (Wai.pathInfo req)

-- | run Apiary monad.
runApiaryTWith :: (Monad actM, Monad m)
               => (forall b. actM b -> IO b)
               -> (Wai.Application -> m a)
               -> Initializer m '[] exts
               -> ApiaryConfig
               -> ApiaryT exts '[] actM m ()
               -> m a
runApiaryTWith runAct run (Initializer ir) conf m = ir NoExtension $ \exts -> do
    wtr <- unApiaryT m (initialEnv conf exts) (\_ w -> return w)
    let doc = docsToDocuments $ writerDoc wtr []
        rtr = writerRouter wtr R.empty
        mw  = allMiddleware exts . writerMw wtr
        mw' = allMiddleware' exts
        app = mw $ execActionT conf exts doc (mw' $ hoistActionT runAct $ routerToAction rtr)
    run $! app

runApiaryWith :: Monad m
              => (Wai.Application -> m a)
              -> Initializer m '[] exts
              -> ApiaryConfig
              -> ApiaryT exts '[] IO m ()
              -> m a
runApiaryWith = runApiaryTWith id

runApiary :: Monad m
          => (Wai.Application -> m a)
          -> ApiaryConfig
          -> ApiaryT '[] '[] IO m ()
          -> m a
runApiary run = runApiaryWith run noExtension

--------------------------------------------------------------------------------

instance Functor (ApiaryT exts prms actM m) where
    fmap f m = ApiaryT $ \env cont ->
        unApiaryT m env $ \a hdr -> hdr `seq` cont (f a) hdr

instance Monad actM => Applicative (ApiaryT exts prms actM m) where
    pure x = ApiaryT $ \_ cont -> cont x mempty
    mf <*> ma = ApiaryT $ \env cont ->
        unApiaryT mf env $ \f hdr  ->
        unApiaryT ma env $ \a hdr' ->
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont (f a) hdr''

instance Monad actM => Monad (ApiaryT exts prms actM m) where
    return x = ApiaryT $ \_ cont -> cont x mempty
    m >>= k = ApiaryT $ \env cont ->
        unApiaryT    m  env $ \a hdr  ->
        unApiaryT (k a) env $ \b hdr' -> 
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont b hdr''

instance Monad actM => MonadTrans (ApiaryT exts prms actM) where
    lift m = ApiaryT $ \_ c -> m >>= \a -> c a mempty

instance (Monad actM, MonadIO m) => MonadIO (ApiaryT exts prms actM m) where
    liftIO m = ApiaryT $ \_ c -> liftIO m >>= \a -> c a mempty

instance (Monad actM, MonadBase b m) => MonadBase b (ApiaryT exts prms actM m) where
    liftBase m = ApiaryT $ \_ c -> liftBase m >>= \a -> c a mempty

instance Monad actM => MonadTransControl (ApiaryT exts prms actM) where
#if MIN_VERSION_monad_control(1,0,0)
    type StT (ApiaryT exts prms actM) a = (a, ApiaryWriter exts actM)
    liftWith f = apiaryT $ \env -> liftM (\a -> (a, mempty)) (f $ \t -> unApiaryT t env (\a w -> return (a,w)))
    restoreT = apiaryT . const
#else
    newtype StT (ApiaryT exts prms actM) a = StTApiary { unStTApiary :: (a, ApiaryWriter exts actM) }
    liftWith f = apiaryT $ \env ->
        liftM (\a -> (a, mempty)) 
        (f $ \t -> liftM StTApiary $ unApiaryT t env (\a w -> return (a,w)))
    restoreT m = apiaryT $ \_ -> liftM unStTApiary m
#endif

instance (Monad actM, MonadBaseControl b m) => MonadBaseControl b (ApiaryT exts prms actM m) where
#if MIN_VERSION_monad_control(1,0,0)
    type StM (ApiaryT exts prms actM m) a = ComposeSt (ApiaryT exts prms actM) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
#else
    newtype StM (ApiaryT exts prms actM m) a = StMApiary { unStMApiary :: ComposeSt (ApiaryT exts prms actM) m a }
    liftBaseWith = defaultLiftBaseWith StMApiary
    restoreM     = defaultRestoreM   unStMApiary
#endif

instance Monad actM => MonadExts exts (ApiaryT exts prms actM m) where
    getExts = envExts <$> getApiaryEnv
    {-# INLINE getExts #-}

--------------------------------------------------------------------------------

getApiaryEnv :: Monad actM => ApiaryT exts prms actM m (ApiaryEnv exts prms actM)
getApiaryEnv = ApiaryT $ \env cont -> cont env mempty

{-# DEPRECATED apiaryExt "use getExt" #-}
-- | get Apiary extension.
apiaryExt :: (Has e exts, Monad actM) => proxy e -> ApiaryT exts prms actM m e
apiaryExt = getExt

-- | get Apiary configuration.
apiaryConfig :: Monad actM => ApiaryT exts prms actM m ApiaryConfig
apiaryConfig = liftM envConfig getApiaryEnv

addRoute :: Monad actM => ApiaryWriter exts actM -> ApiaryT exts prms actM m ()
addRoute r = ApiaryT $ \_ cont -> cont () r

-- | filter by action. since 1.3.0.
focus :: Monad actM
      => (Doc -> Doc)
      -> Maybe Method
      -> (R.Path prms' (ActionT exts '[] actM) () -> R.Path prms (ActionT exts '[] actM) ())
      -> Filter exts actM m prms prms'
focus d meth pth m = ApiaryT $ \ApiaryEnv{..} cont -> unApiaryT m ApiaryEnv
    { envMethod = maybe envMethod Just meth
    , envPath   = envPath . pth
    , envDoc    = envDoc  . d
    , envConfig = envConfig
    , envExts   = envExts
    } cont

-- | splice ActionT to ApiaryT.
action :: Monad actM => ActionT exts prms actM () -> ApiaryT exts prms actM m ()
action a = do
    env <- getApiaryEnv
    let meth = renderMethod <$> envMethod env
        path = envPath env (R.action meth $ flip applyDict a)
    addRoute $ ApiaryWriter
        (R.insert path)
        (envDoc env Action:)
        id

-- | add middleware.
middleware :: Monad actM => Wai.Middleware -> ApiaryT exts prms actM m ()
middleware mw = addRoute (ApiaryWriter id id mw)

--------------------------------------------------------------------------------

insDoc :: (Doc -> Doc) -> Filter' exts actM m
insDoc d m = ApiaryT $ \env cont -> unApiaryT m env
    { envDoc = envDoc env . d } cont

-- | API document group. since 0.12.0.0.
--
-- only top level group recognized.
group :: T.Text -> Filter' exts actM m
group = insDoc . DocGroup

-- | add API document. since 0.12.0.0.
--
-- It use only filters prior document,
-- so you should be placed document directly in front of action.
document :: T.Text -> Filter' exts actM m
document = insDoc . Document

-- | add user defined precondition. since 0.13.0.
precondition :: Html -> Filter' exts actM m
precondition = insDoc . DocPrecondition

-- | ignore next document.
noDoc :: Filter' exts actM m
noDoc = insDoc DocDropNext
