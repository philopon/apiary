{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Data.Apiary.Router
    ( Path, root, exact, leaf
    , raw, fetch, end, any, rest
    , Router
    , empty
    , add, (+|)
    , execute
    ) where

import Prelude hiding(any)
import Control.Monad(MonadPlus(..))
import qualified Network.HTTP.Types as HTTP

import Data.Apiary.Compat(KnownSymbol, symbolVal)
import qualified Data.Apiary.Dict as D
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

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

fetch :: (MonadPlus m, KnownSymbol k, k D.</ d)
      => proxy k -> (T.Text -> Maybe v) -> Path (k D.:= v ': d) m -> Path d m
fetch p f = Param (':' : symbolVal p) go
  where
    go _ [] = mzero
    go d (t:ts) = case f t of
        Nothing -> mzero
        Just v  -> return (D.add p v d, ts)

end :: MonadPlus m => Path d m -> Path d m
end = Param "/" go
  where
    go d [] = return (d, [])
    go _ _  = mzero

any :: Monad m => Path d m -> Path d m
any = Param "**" go
  where
    go d _ = return (d, [])

rest :: (KnownSymbol k, Monad m, k D.</ d)
     => proxy k -> Path (k D.:= [T.Text] ': d) m -> Path d m
rest k = Param (':': symbolVal k ++ "**") go
  where
    go d r = return (D.add k r d, [])

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
        { params    :: Params d m
        , children  :: H.HashMap T.Text (Router d m)
        , methods   :: H.HashMap HTTP.Method (D.Dict d -> m ())
        , anyMethod :: D.Dict d -> m ()
        } -> Router d m

emptyRouter :: MonadPlus m => Router d m
emptyRouter = Router { params    = FNil
                     , children  = H.empty
                     , methods   = H.empty
                     , anyMethod = const mzero
                     }

-- | empty router
empty :: MonadPlus m => Router '[] m
empty = emptyRouter

add' :: MonadPlus m => Path d m -> Router d m -> Router d m
add' (Exact p n) r =
    let c = H.lookupDefault emptyRouter p (children r)
    in r { children = H.insert p (add' n c) (children r) }

add' (Param _ f n) Router{..} = Router
    { params    = FCons f (add' n emptyRouter) params
    , children  = children
    , methods   = methods
    , anyMethod = anyMethod
    }

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
execute' d Router{params, methods, anyMethod} m [] = fetching d m [] params `mplus`
    case H.lookup m methods of
        Nothing -> anyMethod d
        Just f  -> f d

execute' d Router{params, children} m pps@(p:ps) = child `mplus` fetching d m pps params
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
