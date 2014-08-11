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
import Data.Apiary.Document
import Data.Monoid
import Text.Blaze.Html
import qualified Data.Text as T
import qualified Data.ByteString as S
import Data.Apiary.Method
import qualified Data.HashMap.Strict as H

data Router n = Router
    { children   :: H.HashMap T.Text (Router n)
    , capturing  :: Maybe (Router n)
    , anyMatch   :: Maybe (PathMethod n)
    , pathMethod :: PathMethod n
    }

data PathMethod n = PathMethod
    { methodMap :: H.HashMap S.ByteString (ActionT n ())
    , anyMethod :: Maybe (ActionT n ())
    }

emptyRouter :: Router n
emptyRouter = Router H.empty Nothing Nothing emptyPathMethod

emptyPathMethod :: PathMethod n
emptyPathMethod = PathMethod H.empty Nothing

insertRouter :: Monad n => [T.Text] -> Maybe S.ByteString -> [PathElem] -> ActionT n () -> Router n -> Router n
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

data ApiaryEnv n c = ApiaryEnv
    { envFilter :: ActionT n (SList c)
    , envMethod :: Maybe Method
    , envPath   :: [PathElem] -> [PathElem]
    , envConfig :: ApiaryConfig
    , envDoc    :: Doc -> Doc
    }

initialEnv :: Monad n => ApiaryConfig -> ApiaryEnv n '[]
initialEnv conf = ApiaryEnv (return SNil) Nothing id conf id

data ApiaryWriter n = ApiaryWriter
    { writerRouter :: Router n -> Router n
    , writerDoc    :: [Doc] -> [Doc]
    }

instance Monoid (ApiaryWriter n) where
    mempty = ApiaryWriter id id
    ApiaryWriter ra da `mappend` ApiaryWriter rb db = ApiaryWriter (ra . rb) (da . db)

-- | most generic Apiary monad. since 0.8.0.0.
newtype ApiaryT c n m a = ApiaryT { unApiaryT :: forall b.
    ApiaryEnv n c
    -> (a -> ApiaryWriter n -> m b)
    -> m b 
    }
apiaryT :: Monad m
        => (ApiaryEnv n c -> m (a, ApiaryWriter n))
        -> ApiaryT c n m a
apiaryT f = ApiaryT $ \rdr cont -> f rdr >>= \(a,w) -> cont a w

-- | no transformer. (ActionT IO, ApiaryT Identity)
type Apiary c = ApiaryT c IO Identity

routerToAction :: Monad n => Router n -> ActionT n ()
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

runApiaryT :: (Monad n, Monad m) => (forall b. n b -> IO b) -> ApiaryConfig
           -> ApiaryT '[] n m a -> m Application
runApiaryT run conf m = unApiaryT m (initialEnv conf) (\_ w -> return w) >>= \wtr -> do
    let doc = docsToDocuments $ writerDoc wtr []
        rtr = writerRouter wtr emptyRouter
    return $! execActionT conf doc (hoistActionT run $ routerToAction rtr)

runApiary :: ApiaryConfig -> Apiary '[] a -> Application
runApiary conf m = runIdentity $ runApiaryT id conf m

--------------------------------------------------------------------------------

instance Functor (ApiaryT c n m) where
    fmap f m = ApiaryT $ \env cont ->
        unApiaryT m env $ \a hdr -> hdr `seq` cont (f a) hdr

instance Monad n => Applicative (ApiaryT c n m) where
    pure x = ApiaryT $ \_ cont -> cont x mempty
    mf <*> ma = ApiaryT $ \env cont ->
        unApiaryT mf env $ \f hdr  ->
        unApiaryT ma env $ \a hdr' ->
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont (f a) hdr''

instance Monad n => Monad (ApiaryT c n m) where
    return x = ApiaryT $ \_ cont -> cont x mempty
    m >>= k = ApiaryT $ \env cont ->
        unApiaryT    m  env $ \a hdr  ->
        unApiaryT (k a) env $ \b hdr' -> 
        let hdr'' = hdr <> hdr'
        in hdr'' `seq` cont b hdr''

instance Monad n => MonadTrans (ApiaryT c n) where
    lift m = ApiaryT $ \_ c -> m >>= \a -> c a mempty

instance (Monad n, MonadIO m) => MonadIO (ApiaryT c n m) where
    liftIO m = ApiaryT $ \_ c -> liftIO m >>= \a -> c a mempty

instance (Monad n, MonadBase b m) => MonadBase b (ApiaryT c n m) where
    liftBase m = ApiaryT $ \_ c -> liftBase m >>= \a -> c a mempty

instance Monad n => MonadTransControl (ApiaryT c n) where
    newtype StT (ApiaryT c n) a = StTApiary' { unStTApiary' :: (a, ApiaryWriter n) }
    liftWith f = apiaryT $ \env ->
        liftM (\a -> (a, mempty)) 
        (f $ \t -> liftM StTApiary' $ unApiaryT t env (\a w -> return (a,w)))
    restoreT m = apiaryT $ \_ -> liftM unStTApiary' m

instance (Monad n, MonadBaseControl b m) => MonadBaseControl b (ApiaryT c n m) where
    newtype StM (ApiaryT c n m) a = StMApiary' { unStMApiary' :: ComposeSt (ApiaryT c n) m a }
    liftBaseWith = defaultLiftBaseWith StMApiary'
    restoreM     = defaultRestoreM   unStMApiary'

--------------------------------------------------------------------------------

getApiaryEnv :: Monad n => ApiaryT c n m (ApiaryEnv n c)
getApiaryEnv = ApiaryT $ \env cont -> cont env mempty

apiaryConfig :: Monad n => ApiaryT c n m ApiaryConfig
apiaryConfig = liftM envConfig getApiaryEnv

addRoute :: Monad n => ApiaryWriter n -> ApiaryT c n m ()
addRoute r = ApiaryT $ \_ cont -> cont () r

-- | filter by action. since 0.6.1.0.
focus :: Monad n
      => (Doc -> Doc)
      -> (SList c -> ActionT n (SList c'))
      -> ApiaryT c' n m a -> ApiaryT c n m a
focus d g m = focus' d Nothing id g m

focus' :: Monad n
       => (Doc -> Doc)
       -> Maybe Method
       -> ([PathElem] -> [PathElem])
       -> (SList c -> ActionT n (SList c'))
       -> ApiaryT c' n m a -> ApiaryT c n m a
focus' d meth pth g m = ApiaryT $ \env cont -> unApiaryT m env 
    { envFilter = envFilter env >>= g 
    , envMethod = maybe (envMethod env) Just meth
    , envPath   = envPath env . pth
    , envDoc    = envDoc env  . d
    } cont

-- | splice ActionT ApiaryT.
action :: Monad n => Fn (Reverse c) (ActionT n ()) -> ApiaryT c n m ()
action = action' . apply

-- | like action. but not apply arguments. since 0.8.0.0.
action' :: Monad n => (SList (Reverse c) -> ActionT n ()) -> ApiaryT c n m ()
action' a = do
    env <- getApiaryEnv
    addRoute $ ApiaryWriter
        (insertRouter
            (rootPattern $ envConfig env)
            (renderMethod <$> envMethod env)
            (envPath env [])
            (envFilter env >>= \c -> a (sReverse c)))
        (envDoc env Action:)
--------------------------------------------------------------------------------

insDoc :: (Doc -> Doc) -> ApiaryT c n m a -> ApiaryT c n m a
insDoc d m = ApiaryT $ \env cont -> unApiaryT m env
    { envDoc = envDoc env . d } cont

-- | API document group. since 0.12.0.0.
--
-- only top level group recognized.
group :: T.Text -> ApiaryT c n m a -> ApiaryT c n m a
group = insDoc . DocGroup

-- | add API document. since 0.12.0.0.
--
-- It use only filters prior document,
-- so you should be placed document directly in front of action.
document :: T.Text -> ApiaryT c n m a -> ApiaryT c n m a
document = insDoc . Document

-- | add user defined precondition. since 0.13.0.
precondition :: Html -> ApiaryT c n m a -> ApiaryT c n m a
precondition = insDoc . DocPrecondition

noDoc :: ApiaryT c n m a -> ApiaryT c n m a
noDoc = insDoc DocDropNext

--------------------------------------------------------------------------------

{-# DEPRECATED actionWithPreAction "use action'" #-}
-- | execute action before main action. since 0.4.2.0
actionWithPreAction :: Monad n => (SList (Reverse xs) -> ActionT n a)
                    -> Fn (Reverse xs) (ActionT n ()) -> ApiaryT xs n m ()
actionWithPreAction pa a = do
    action' $ \c -> pa c >> apply a c
