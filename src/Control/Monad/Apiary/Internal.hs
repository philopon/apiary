{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Apiary.Internal where

import qualified Network.Wai as Wai

import Control.Applicative(Applicative(..), (<$>))
import Control.Monad(liftM, MonadPlus(..))
import Control.Monad.Trans(MonadIO(liftIO), MonadTrans(lift))
import Control.Monad.Trans.Control
    ( MonadTransControl(..), MonadBaseControl(..)
    , ComposeSt, defaultLiftBaseWith, defaultRestoreM
    )
import Control.Monad.Base(MonadBase(..))
import Control.Monad.Apiary.Action.Internal
    (ActionT, ApiaryConfig, getRequest
    , modifyState, actionFetches, Extensions(NoExtension)
    , execActionT, hoistActionT, applyDict, rootPattern
    )

import qualified Data.Apiary.Dict as D
import Data.Apiary.Method(Method, renderMethod)
import Data.Apiary.Extension ( Has, MonadExts(..), getExt, noExtension )
import Data.Apiary.Extension.Internal(Initializer(..), allMiddleware, allMiddleware')
import Data.Apiary.Document.Internal(Doc(..), docsToDocuments)
import Data.Monoid(Monoid(..), (<>))

import Data.List(foldl')
import Text.Blaze.Html(Html)
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as H

data Router exts actM = Router
    { children   :: H.HashMap T.Text (Router exts actM)
    , capturing  :: Maybe (Router exts actM)
    , restMatch  :: Maybe (PathMethod exts actM)
    , pathMethod :: PathMethod exts actM
    }

type ActionT' exts actM a = ActionT exts '[] actM a

data PathMethod exts actM = PathMethod
    { methodMap :: H.HashMap S.ByteString (ActionT' exts actM ())
    , anyMethod :: Maybe (ActionT' exts actM ())
    }

emptyRouter :: Router exts actM
emptyRouter = Router H.empty Nothing Nothing emptyPathMethod

emptyPathMethod :: PathMethod exts actM
emptyPathMethod = PathMethod H.empty Nothing

insertRouter :: Monad actM => [T.Text] -> Maybe S.ByteString -> [PathElem]
             -> ActionT' exts actM () -> Router exts actM -> Router exts actM
insertRouter rootPat mbMethod paths act = loop paths
  where
    loop [EndPath] (Router cln cap anp pm) =
        Router cln cap anp $ insPathMethod pm

    loop [] (Router cln cap anp pm) =
        Router cln cap (Just . insPathMethod $ maybe emptyPathMethod id anp) pm

    loop (mbp:ps) rtr@(Router cln cap anp pm) = case mbp of
        FetchPath -> Router cln (Just $ loop ps (maybe emptyRouter id cap)) anp pm
        Exact p   -> Router (adjust' (loop ps) p cln) cap anp pm
        EndPath   -> loop ps rtr
        RestPath  -> Router cln cap (Just . insPathMethod $ maybe emptyPathMethod id anp) pm
        RootPath  -> let cln' = foldl' (flip $ adjust' (loop [EndPath])) cln rootPat
                     in loop [EndPath] $ Router cln' cap anp pm

    adjust' f k h = H.adjust f k (H.insertWith (\_ old -> old) k emptyRouter h)

    insPathMethod (PathMethod mm am) = case mbMethod of
        Nothing -> PathMethod mm (Just $ maybe act (mplus act) am)
        Just m  -> PathMethod (H.insertWith mplus m act mm) am

data PathElem = Exact {-# UNPACK #-} !T.Text
              | FetchPath
              | RootPath
              | EndPath
              | RestPath

data ApiaryEnv exts prms actM = ApiaryEnv
    { envFilter :: ActionT' exts actM (D.Dict prms)
    , envMethod :: Maybe Method
    , envPath   :: [PathElem] -> [PathElem]
    , envConfig :: ApiaryConfig
    , envDoc    :: Doc -> Doc
    , envExts   :: Extensions exts
    }

initialEnv :: Monad actM => ApiaryConfig -> Extensions exts -> ApiaryEnv exts '[] actM
initialEnv conf = ApiaryEnv (return D.empty) Nothing id conf id

data ApiaryWriter exts actM = ApiaryWriter
    { writerRouter :: Router exts actM -> Router exts actM
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

routerToAction :: Monad actM => Router exts actM -> ActionT' exts actM ()
routerToAction router = getRequest >>= go
  where
    go req = loop id router (Wai.pathInfo req)
      where
        method = Wai.requestMethod req

        pmAction nxt (PathMethod mm am) =
            let a = maybe nxt id am
            in maybe a (`mplus` a) $ H.lookup method mm

        loop fch (Router _ _ anp pm) [] = do
            modifyState (\s -> s { actionFetches = fch [] } )
            pmAction (maybe mzero (pmAction mzero) anp) pm 

        loop fch (Router c mbcp anp _) (p:ps) = case mbcp of
            Nothing -> cld ana
            Just cp -> cld $ loop (fch . (p:)) cp ps `mplus` ana
          where
            ana = do
                modifyState (\s -> s {actionFetches = fch $ p:ps} ) 
                maybe mzero (pmAction mzero) anp
            cld nxt = case H.lookup p c of
                Nothing -> nxt
                Just cd -> loop fch cd ps `mplus` nxt

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
        rtr = writerRouter wtr emptyRouter
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
    newtype StT (ApiaryT exts prms actM) a = StTApiary' { unStTApiary' :: (a, ApiaryWriter exts actM) }
    liftWith f = apiaryT $ \env ->
        liftM (\a -> (a, mempty)) 
        (f $ \t -> liftM StTApiary' $ unApiaryT t env (\a w -> return (a,w)))
    restoreT m = apiaryT $ \_ -> liftM unStTApiary' m

instance (Monad actM, MonadBaseControl b m) => MonadBaseControl b (ApiaryT exts prms actM m) where
    newtype StM (ApiaryT exts prms actM m) a = StMApiary' { unStMApiary' :: ComposeSt (ApiaryT exts prms actM) m a }
    liftBaseWith = defaultLiftBaseWith StMApiary'
    restoreM     = defaultRestoreM   unStMApiary'

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

-- | filter by action. since 0.6.1.0.
focus :: Monad actM
      => (Doc -> Doc)
      -> ActionT exts prms actM (D.Dict prms')
      -> ApiaryT exts prms' actM m () -> ApiaryT exts prms actM m ()
focus d g m = focus' d Nothing id g m

focus' :: Monad actM
       => (Doc -> Doc)
       -> Maybe Method
       -> ([PathElem] -> [PathElem])
       -> ActionT exts prms actM (D.Dict prms')
       -> ApiaryT exts prms' actM m () -> ApiaryT exts prms actM m ()
focus' d meth pth g m = ApiaryT $ \env cont -> unApiaryT m env 
    { envFilter = envFilter env >>= flip applyDict g
    , envMethod = maybe (envMethod env) Just meth
    , envPath   = envPath env . pth
    , envDoc    = envDoc env  . d
    } cont

-- | splice ActionT to ApiaryT.
action :: Monad actM => ActionT exts prms actM () -> ApiaryT exts prms actM m ()
action a = do
    env <- getApiaryEnv
    addRoute $ ApiaryWriter
        (insertRouter
            (rootPattern $ envConfig env)
            (renderMethod <$> envMethod env)
            (envPath env [])
            (envFilter env >>= flip applyDict a))
        (envDoc env Action:)
        id

-- | add middleware.
middleware :: Monad actM => Wai.Middleware -> ApiaryT exts prms actM m ()
middleware mw = addRoute (ApiaryWriter id id mw)

--------------------------------------------------------------------------------

insDoc :: (Doc -> Doc) -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
insDoc d m = ApiaryT $ \env cont -> unApiaryT m env
    { envDoc = envDoc env . d } cont

-- | API document group. since 0.12.0.0.
--
-- only top level group recognized.
group :: T.Text -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
group = insDoc . DocGroup

-- | add API document. since 0.12.0.0.
--
-- It use only filters prior document,
-- so you should be placed document directly in front of action.
document :: T.Text -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
document = insDoc . Document

-- | add user defined precondition. since 0.13.0.
precondition :: Html -> ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
precondition = insDoc . DocPrecondition

-- | ignore next document.
noDoc :: ApiaryT exts prms actM m () -> ApiaryT exts prms actM m ()
noDoc = insDoc DocDropNext
