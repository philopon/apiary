{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.Apiary.Internal where

import Network.Wai

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Apiary.Action.Internal

import Data.List
import Data.Apiary.SList
import Data.Apiary.Extension
import Data.Apiary.Extension.Internal
import Data.Apiary.Document
import Data.Monoid hiding (All)
import Text.Blaze.Html
import qualified Data.Text as T
import qualified Data.ByteString as S
import Data.Apiary.Method
import qualified Data.HashMap.Strict as H

data Router exts actM = Router
    { children   :: H.HashMap T.Text (Router exts actM)
    , capturing  :: Maybe (Router exts actM)
    , anyMatch   :: Maybe (PathMethod exts actM)
    , pathMethod :: PathMethod exts actM
    }

data PathMethod exts actM = PathMethod
    { methodMap :: H.HashMap S.ByteString (ActionT exts actM ())
    , anyMethod :: Maybe (ActionT exts actM ())
    }

emptyRouter :: Router exts actM
emptyRouter = Router H.empty Nothing Nothing emptyPathMethod

emptyPathMethod :: PathMethod exts actM
emptyPathMethod = PathMethod H.empty Nothing

insertRouter :: Monad actM => [T.Text] -> Maybe S.ByteString -> [PathElem]
             -> ActionT exts actM () -> Router exts actM -> Router exts actM
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
        AnyPath   -> Router cln cap (Just . insPathMethod $ maybe emptyPathMethod id anp) pm
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
              | AnyPath

data ApiaryEnv exts prms actM = ApiaryEnv
    { envFilter :: ActionT exts actM (SList prms)
    , envMethod :: Maybe Method
    , envPath   :: [PathElem] -> [PathElem]
    , envConfig :: ApiaryConfig
    , envDoc    :: Doc -> Doc
    , envExts   :: Extensions exts
    }

initialEnv :: Monad actM => Extensions exts -> ApiaryConfig -> ApiaryEnv exts '[] actM
initialEnv exts conf = ApiaryEnv (return SNil) Nothing id conf id exts

initialEnv' :: Monad actM => ApiaryConfig -> ApiaryEnv '[] '[] actM
initialEnv' conf = ApiaryEnv (return SNil) Nothing id conf id noExtension

data ApiaryWriter exts actM = ApiaryWriter
    { writerRouter :: Router exts actM -> Router exts actM
    , writerDoc    :: [Doc] -> [Doc]
    }

instance Monoid (ApiaryWriter exts actM) where
    mempty = ApiaryWriter id id
    ApiaryWriter ra da `mappend` ApiaryWriter rb db = ApiaryWriter (ra . rb) (da . db)

-- | most generic Apiary monad. since 0.8.0.0.
newtype ApiaryT exts prms actM m a = ApiaryT { unApiaryT :: forall b.
    ApiaryEnv exts prms actM
    -> (a -> ApiaryWriter exts actM -> m b)
    -> m b 
    }
apiaryT :: Monad m
        => (ApiaryEnv exts prms actM -> m (a, ApiaryWriter exts actM))
        -> ApiaryT exts prms actM m a
apiaryT f = ApiaryT $ \rdr cont -> f rdr >>= \(a,w) -> cont a w

-- | no transformer. (ActionT IO, ApiaryT Identity)
type Apiary exts prms = ApiaryT exts prms IO Identity

routerToAction :: Monad actM => Router exts actM -> ActionT exts actM ()
routerToAction router = getRequest >>= go
  where
    go req = loop id router (pathInfo req)
      where
        method = requestMethod req

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
            ana = maybe mzero (pmAction mzero) anp
            cld nxt = case H.lookup p c of
                Nothing -> nxt
                Just cd -> loop fch cd ps `mplus` nxt

runApiaryTWith :: (All Extension exts, Monad actM, Monad m) => (forall b. actM b -> IO b) -> ApiaryConfig
               -> Extensions exts -> ApiaryT exts '[] actM m a -> m Application
runApiaryTWith run conf exts m = unApiaryT m (initialEnv exts conf) (\_ w -> return w) >>= \wtr -> do
    let doc = docsToDocuments $ writerDoc wtr []
        rtr = writerRouter wtr emptyRouter
    return $! execActionT conf exts doc (hoistActionT run $ routerToAction rtr)

runApiaryT :: (Monad actM, Monad m) => (forall b. actM b -> IO b) -> ApiaryConfig
           -> ApiaryT '[] '[] actM m a -> m Application
runApiaryT run conf = runApiaryTWith run conf noExtension


runApiaryWith :: All Extension exts => ApiaryConfig -> Extensions exts -> Apiary exts '[] a -> Application
runApiaryWith conf exts m = runIdentity $ runApiaryTWith id conf exts m

runApiary :: ApiaryConfig -> Apiary '[] '[] a -> Application
runApiary conf = runApiaryWith conf noExtension

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

--------------------------------------------------------------------------------

getApiaryEnv :: Monad actM => ApiaryT exts prms actM m (ApiaryEnv exts prms actM)
getApiaryEnv = ApiaryT $ \env cont -> cont env mempty

apiaryConfig :: Monad actM => ApiaryT exts prms actM m ApiaryConfig
apiaryConfig = liftM envConfig getApiaryEnv

apiaryExt :: (Extension e, Member e exts, Monad actM) => proxy e -> ApiaryT exts prms actM m e
apiaryExt p = liftM (get p . unExtensions . envExts) getApiaryEnv

addRoute :: Monad actM => ApiaryWriter exts actM -> ApiaryT exts prms actM m ()
addRoute r = ApiaryT $ \_ cont -> cont () r

-- | filter by action. since 0.6.1.0.
focus :: Monad actM
      => (Doc -> Doc)
      -> (SList prms -> ActionT exts actM (SList prms'))
      -> ApiaryT exts prms' actM m a -> ApiaryT exts prms actM m a
focus d g m = focus' d Nothing id g m

focus' :: Monad actM
       => (Doc -> Doc)
       -> Maybe Method
       -> ([PathElem] -> [PathElem])
       -> (SList prms -> ActionT exts actM (SList prms'))
       -> ApiaryT exts prms' actM m a -> ApiaryT exts prms actM m a
focus' d meth pth g m = ApiaryT $ \env cont -> unApiaryT m env 
    { envFilter = envFilter env >>= g 
    , envMethod = maybe (envMethod env) Just meth
    , envPath   = envPath env . pth
    , envDoc    = envDoc env  . d
    } cont

-- | splice ActionT ApiaryT.
action :: Monad actM => Fn prms (ActionT exts actM ()) -> ApiaryT exts prms actM m ()
action = action' . apply

-- | like action. but not apply arguments. since 0.8.0.0.
action' :: Monad actM => (SList prms -> ActionT exts actM ()) -> ApiaryT exts prms actM m ()
action' a = do
    env <- getApiaryEnv
    addRoute $ ApiaryWriter
        (insertRouter
            (rootPattern $ envConfig env)
            (renderMethod <$> envMethod env)
            (envPath env [])
            (envFilter env >>= \prms -> a prms))
        (envDoc env Action:)
--------------------------------------------------------------------------------

insDoc :: (Doc -> Doc) -> ApiaryT exts prms actM m a -> ApiaryT exts prms actM m a
insDoc d m = ApiaryT $ \env cont -> unApiaryT m env
    { envDoc = envDoc env . d } cont

-- | API document group. since 0.12.0.0.
--
-- only top level group recognized.
group :: T.Text -> ApiaryT exts prms actM m a -> ApiaryT exts prms actM m a
group = insDoc . DocGroup

-- | add API document. since 0.12.0.0.
--
-- It use only filters prior document,
-- so you should be placed document directly in front of action.
document :: T.Text -> ApiaryT exts prms actM m a -> ApiaryT exts prms actM m a
document = insDoc . Document

-- | add user defined precondition. since 0.13.0.
precondition :: Html -> ApiaryT exts prms actM m a -> ApiaryT exts prms actM m a
precondition = insDoc . DocPrecondition

noDoc :: ApiaryT exts prms actM m a -> ApiaryT exts prms actM m a
noDoc = insDoc DocDropNext

--------------------------------------------------------------------------------

{-# DEPRECATED actionWithPreAction "use action'" #-}
-- | execute action before main action. since 0.4.2.0
actionWithPreAction :: Monad actM => (SList xs -> ActionT exts actM a)
                    -> Fn xs (ActionT exts actM ()) -> ApiaryT exts xs actM m ()
actionWithPreAction pa a = action' $ \prms -> pa prms >> apply a prms
