{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Web.Apiary.Helics
    ( Helics
    -- * initializer
    , HelicsConfig(..)
    , initHelics
    -- * action
    -- ** raw
    , transactionId
    -- ** metric
    , recordMetric
    -- ** transaction
    , addAttribute
    -- ** segment
    , genericSegment
    , datastoreSegment
    , externalSegment
    -- * reexports
    , H.autoScope
    , H.rootSegment
    , H.TransactionId
    , H.SegmentId
    , H.Operation(..)
    , H.DatastoreSegment(..)
    ) where

import Control.Concurrent
import Control.Applicative
import Control.Monad.Apiary.Action
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Network.Wai
import qualified Network.Helics as H
import qualified Network.Helics.Wai.Safe as Safe

import Data.Apiary.Compat
import Data.Apiary.Extension
import Data.Default.Class
import qualified Data.Vault.Lazy as V
import qualified Data.ByteString as S

data Helics = Helics (V.Key H.TransactionId) ThreadId
instance Extension Helics where
    extMiddleware (Helics k _) = Safe.helics k

data HelicsConfig = HelicsConfig
    { licenseKey       :: S.ByteString
    , appName          :: S.ByteString
    , language         :: S.ByteString
    , languageVersion  :: S.ByteString
    , statusCallback   :: Maybe (H.StatusCode -> IO ())
    , samplerFrequency :: Int
    }

instance Default HelicsConfig where
    def = HelicsConfig { licenseKey       = H.licenseKey def
                       , appName          = H.appName def
                       , language         = H.language def
                       , languageVersion  = H.languageVersion def
                       , statusCallback   = H.statusCallback def
                       , samplerFrequency = 20
                       }

conv :: HelicsConfig -> H.HelicsConfig
conv c = def { H.licenseKey      = licenseKey c
             , H.appName         = appName c
             , H.language        = language c
             , H.languageVersion = languageVersion c
             , H.statusCallback  = statusCallback c
             }

initHelics :: (MonadBaseControl IO m, MonadIO m) => HelicsConfig -> Initializer' m Helics
initHelics cnf = initializerBracket' $ \m -> do
    k <- liftIO $ V.newKey
    control $ \run -> H.withHelics (conv cnf) $ run $ do
        tid <- liftIO $ forkIO $ H.sampler (samplerFrequency cnf)
        m (Helics k tid)

recordMetric :: MonadIO m => S.ByteString -> Double
             -> ActionT exts prms m ()
recordMetric n v = liftIO $ H.recordMetric n v

transactionId :: (Has Helics exts, Monad m)
              => ActionT exts prms m H.TransactionId
transactionId = do
    Helics key _ <- getExt Proxy
    maybe (error "apiary-helics: vault value not found.") id .
        V.lookup key . vault <$> getRequest

addAttribute :: (MonadIO m, Has Helics exts)
             => S.ByteString -> S.ByteString -> ActionT exts prms m ()
addAttribute key val = do
    tid <- transactionId
    liftIO $ H.addAttribute key val tid

genericSegment :: (Has Helics exts, MonadBaseControl IO m)
               => H.SegmentId  -- ^ parent segment id
               -> S.ByteString -- ^ name of represent segment
               -> ActionT exts prms m a
               -> ActionT exts prms m a
genericSegment sid name act = do
    tid <- transactionId
    control $ \run -> H.genericSegment sid name (run act) tid

datastoreSegment :: (Has Helics exts, MonadBaseControl IO m)
                 => H.SegmentId  -- ^ parent segment id
                 -> H.DatastoreSegment
                 -> ActionT exts prms m a
                 -> ActionT exts prms m a
datastoreSegment sid ds act = do
    tid <- transactionId
    control $ \run -> H.datastoreSegment sid ds (run act) tid

externalSegment :: (Has Helics exts, MonadBaseControl IO m)
                => H.SegmentId  -- ^ parent segment id
                -> S.ByteString -- ^ host of segment
                -> S.ByteString -- ^ name of segment
                -> ActionT exts prms m a
                -> ActionT exts prms m a
externalSegment sid host name act = do
    tid <- transactionId
    control $ \run -> H.externalSegment sid host name (run act) tid
