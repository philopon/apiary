{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
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
newtype ApiaryT c n m a = ApiaryT { unApiaryT ::
    ApiaryEnv n c
    -> m (ApiaryWriter n, a)
    }

-- | no transformer. (ActionT IO, ApiaryT Identity)
type Apiary c = ApiaryT c IO Identity

routerToAction :: Monad n => Router n -> ActionT n ()
routerToAction router = getRequest >>= go
  where
    go req = loop id router (pathInfo req)
      where
        method = requestMethod req

        pmAction (PathMethod mm am) =
            maybe mzero id (H.lookup method mm) `mplus` maybe mzero id am

        loop fch (Router _ _ anp pm) [] = do
            modifyState (\s -> s { actionFetches = fch [] } )
            pmAction pm `mplus` maybe mzero pmAction anp

        loop fch (Router c mbcp anp _) (p:ps) = case mbcp of
            Nothing -> cld `mplus` maybe mzero pmAction anp
            Just cp -> cld `mplus` loop (fch . (p:)) cp ps `mplus` maybe mzero pmAction anp
          where
            cld = case H.lookup p c of
                Nothing -> mzero
                Just cd -> loop fch cd ps

runApiaryT :: (Monad n, Monad m) => (forall b. n b -> IO b) -> ApiaryConfig
           -> ApiaryT '[] n m a -> m Application
runApiaryT run conf m = unApiaryT m (initialEnv conf) >>= \(wtr, _) -> do
    let doc = docsToDocuments $ writerDoc wtr []
        rtr = writerRouter wtr emptyRouter
    return $ execActionT conf doc (hoistActionT run $ routerToAction rtr)

runApiary :: ApiaryConfig -> Apiary '[] a -> Application
runApiary conf m = runIdentity $ runApiaryT id conf m

--------------------------------------------------------------------------------

instance Functor m => Functor (ApiaryT c n m) where
    fmap f m = ApiaryT $ \env ->
        fmap f <$> unApiaryT m env

instance (Functor m, Monad m, Monad n) => Applicative (ApiaryT c n m) where
    pure x = ApiaryT $ \_ -> return (mempty, x)
    mf <*> ma = ApiaryT $ \env ->
        unApiaryT mf env >>= \(w,  f) ->
        unApiaryT ma env >>= \(w', a) ->
        let w'' = w <> w'
        in w'' `seq` return (w'', f a)

instance (Functor m, Monad m, Monad n) => Monad (ApiaryT c n m) where
    return x = ApiaryT $ \_ -> return (mempty, x)
    m >>= k = ApiaryT $ \env ->
        unApiaryT    m  env >>= \(w,  a) ->
        unApiaryT (k a) env >>= \(w', b) ->
        let w'' = w <> w'
        in w'' `seq` return (w'', b)

instance Monad n => MonadTrans (ApiaryT c n) where
    lift m = ApiaryT $ \_ -> (mempty,) `liftM` m

instance (Functor m, MonadIO m, Monad n) => MonadIO (ApiaryT c n m) where
    liftIO m = ApiaryT $ \_ -> (mempty,) `liftM` liftIO m

instance (Monad n, MonadBase b m) => MonadBase b (ApiaryT c n m) where
    liftBase m = ApiaryT $ \_ -> (mempty,) `liftM` liftBase m

instance Monad n => MonadTransControl (ApiaryT c n) where
    newtype StT (ApiaryT c n) a = StTApiary' { unStTApiary' :: (ApiaryWriter n, a) }
    liftWith f = ApiaryT $ \env ->
        liftM (mempty,) (f $ \t -> liftM StTApiary' $ unApiaryT t env)
    restoreT m = ApiaryT $ \_ -> liftM unStTApiary' m

instance (Monad n, MonadBaseControl b m) => MonadBaseControl b (ApiaryT c n m) where
    newtype StM (ApiaryT c n m) a = StMApiary' { unStMApiary' :: ComposeSt (ApiaryT c n) m a }
    liftBaseWith = defaultLiftBaseWith StMApiary'
    restoreM     = defaultRestoreM   unStMApiary'

--------------------------------------------------------------------------------

getApiaryEnv :: (Monad m, Monad n) => ApiaryT c n m (ApiaryEnv n c)
getApiaryEnv = ApiaryT $ \env -> return (mempty, env)

apiaryConfig :: (Functor m, Monad m, Monad n) => ApiaryT c n m ApiaryConfig
apiaryConfig = liftM envConfig getApiaryEnv

addRoute :: (Monad m, Monad n) => ApiaryWriter n -> ApiaryT c n m ()
addRoute r = ApiaryT $ \_ -> return (r, ())

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
focus' d meth pth g m = ApiaryT $ \env -> unApiaryT m env 
    { envFilter = envFilter env >>= g 
    , envMethod = maybe (envMethod env) Just meth
    , envPath   = envPath env . pth
    , envDoc    = envDoc env  . d
    }

-- | splice ActionT ApiaryT.
action :: (Functor m, Monad m, Monad n) => Fn c (ActionT n ()) -> ApiaryT c n m ()
action = action' . apply

-- | like action. but not apply arguments. since 0.8.0.0.
action' :: (Functor m, Monad m, Monad n) => (SList c -> ActionT n ()) -> ApiaryT c n m ()
action' a = do
    env <- getApiaryEnv
    addRoute $ ApiaryWriter
        (insertRouter
            (rootPattern $ envConfig env)
            (renderMethod <$> envMethod env)
            (envPath env [])
            (envFilter env >>= \c -> a c))
        (envDoc env Action:)
--------------------------------------------------------------------------------

insDoc :: (Doc -> Doc) -> ApiaryT c n m a -> ApiaryT c n m a
insDoc d m = ApiaryT $ \env -> unApiaryT m env
    { envDoc = envDoc env . d }

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
actionWithPreAction :: (Functor m, Monad m, Monad n) => (SList xs -> ActionT n a)
                    -> Fn xs (ActionT n ()) -> ApiaryT xs n m ()
actionWithPreAction pa a = do
    action' $ \c -> pa c >> apply a c
