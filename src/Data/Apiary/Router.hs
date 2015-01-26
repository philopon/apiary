{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Apiary.Router
    ( Path, root, exact, leaf
    , raw, fetch
    , Router
    , empty
    , add, (+|)
    , execute
    , test
    ) where

import Control.Monad(MonadPlus(..))
import GHC.TypeLits(KnownSymbol, symbolVal)
import qualified Network.HTTP.Types as HTTP

import qualified Data.Apiary.Dict as D
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H


import Data.Proxy
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

data Params d m where
    FCons :: (D.Dict d -> [T.Text] -> m (D.Dict d', [T.Text]))
          -> Router d' m
          -> Params d m -> Params d m
    FNil  :: Params d m

-- | routing path.
--
-- root $ exact "foo" $ fetch (Proxy :: Proxy "key") Just $ leaf Nothing (\d -> print (D.get (Proxy :: Proxy "key") d))
data Path d m where
    Exact :: T.Text -> Path d m -> Path d m

    Param :: String -> (D.Dict d -> [T.Text] -> m (D.Dict d', [T.Text]))
          -> Path d' m -> Path d m

    Leaf  :: Maybe HTTP.Method
          -> (D.Dict d -> m ()) -> Path d m

-- | root
root :: Path '[] m -> Path '[] m
root = id

-- | exact matching path
exact :: T.Text -> Path d m -> Path d m
exact = Exact

raw :: String -> (D.Dict d -> [T.Text] -> m (D.Dict d', [T.Text]))
    -> Path d' m -> Path d m
raw = Param

fetch :: (MonadPlus m, KnownSymbol k, D.NotMember k d)
      => proxy k -> (T.Text -> Maybe v) -> Path (k D.:= v ': d) m -> Path d m
fetch p f = Param (':' : symbolVal p) go
  where
    go _ [] = mzero
    go d (t:ts) = case f t of
        Nothing -> mzero
        Just v  -> return (D.insert p v d, ts)

-- | action
leaf :: Maybe HTTP.Method -- ^ if Nothing, any method allowed.
     -> (D.Dict d -> m ()) -> Path d m
leaf  = Leaf

instance Show (Path d m) where
    show (Exact t ps) = '/': T.unpack t ++ show ps
    show (Param l _ ps) = "/" ++ l ++ show ps
    show (Leaf m _) = "/[" ++ maybe "*" show m ++ "]"

-- | router
data Router d m where
    Router ::
        { children  :: !(H.HashMap T.Text (Router d m))
        , params    :: !(Params d m)
        , methods   :: !(H.HashMap HTTP.Method (D.Dict d -> m ()))
        , anyMethod :: !(D.Dict d -> m ())
        } -> Router d m

emptyRouter :: MonadPlus m => Router d m
emptyRouter = Router H.empty FNil H.empty (const mzero)

-- | empty router
empty :: MonadPlus m => Router '[] m
empty = emptyRouter

add' :: MonadPlus m => Path d m -> Router d m -> Router d m
add' (Exact p n) r =
    let c = H.lookupDefault emptyRouter p (children r)
    in r { children = H.insert p (add' n c) (children r) }

add' (Param _ f n) Router{..} =
    Router children (FCons f (add' n emptyRouter) params) methods anyMethod

add' (Leaf (Just m) n) r = 
    let c = case H.lookup m (methods r) of
            Nothing -> \d -> n d
            Just p  -> \d -> p d `mplus` n d
    in r { methods = H.insert m c (methods r) }

add' (Leaf Nothing n) r =
    r { anyMethod = \d -> anyMethod r d `mplus` n d }

-- | insert path to router
add :: MonadPlus m => Path '[] m -> Router '[] m -> Router '[] m
add = add'

(+|) :: MonadPlus m => Path '[] m -> Router '[] m -> Router '[] m
(+|) = add

infixr `add`
infixr +|

-- | execute router
execute :: MonadPlus m => Router '[] m -> HTTP.Method -> [T.Text] -> m ()
execute = execute' D.empty

execute' :: MonadPlus m => D.Dict d -> Router d m -> HTTP.Method -> [T.Text] -> m ()
execute' d Router{params, methods, anyMethod} m [] =
    fetching d m [] params `mplus` case H.lookup m methods of
        Nothing -> anyMethod d
        Just f  -> f d

execute' d Router{params, children} m pps@(p:ps) = fetching d m pps params `mplus` child
  where
    child = case H.lookup p children of
        Nothing -> mzero
        Just c  -> execute' d c m ps

fetching :: MonadPlus m => D.Dict d -> HTTP.Method -> [T.Text] -> Params d m -> m ()
fetching d m pps = loop
  where
    loop FNil = mzero
    loop (FCons f r o) =
        do (d', pps') <- f d pps
           execute' d' r m pps'
        `mplus` loop o

test :: Router '[] (MaybeT IO)
test = e +| d +| c +| b +| a +| empty
  where
    a = leaf Nothing (\_ -> liftIO $ putStrLn "A" :: MaybeT IO ())
    b = exact "foo" $ leaf Nothing (\_ -> liftIO $ putStrLn "B" :: MaybeT IO ())
    c = fetch (Proxy :: Proxy "neko") (\i -> if i == "ok" then Just (12::Int) else Nothing) 
        $ leaf Nothing (\dict -> liftIO $ putStrLn ("C:" ++ show (D.get (Proxy :: Proxy "neko") dict)) :: MaybeT IO ())
    d = exact "get" $ leaf (Just "GET") (\_ -> liftIO $ putStrLn "D" :: MaybeT IO ())
    e = exact "dic"
        $ raw "dict" (\dict p -> liftIO (putStrLn "add dict") >> return (D.insert (Proxy :: Proxy "d") (32::Int) dict, p)) 
        $ leaf (Just "GET") (\dict -> liftIO $ putStrLn ("E:" ++ show (D.get (Proxy :: Proxy "d") dict)) :: MaybeT IO ())
