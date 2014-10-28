{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Monitor
  ( withMonitor
  -- * The Monitor
  , Monitor(..)
  , HasMonitor(..)
  -- * Gauges
  , Gauge(..)
  , gauge, gaugeM
  -- * Counters
  , Counter(..)
  , counter, counterM
  -- * Labels
  , Label(..)
  , label, labelM
  -- * Compatibilty with EKG
  , Server
  , withServer
  , forkServer
  -- * Modifiers
  , Setting(..)
  , Updating(..)
  , Gauged(..)
  , Incremental(..)
  -- * Options
  , MonitorOptions(..)
  , HasMonitorOptions(..)
  , parseMonitorOptions
  , monitorUri
  ) where

import Control.Exception
import Control.Lens hiding (Setting)
import Control.Monad.Trans
import Control.Monad.Reader
import Data.ByteString.Lens
import Data.Data
import Data.Default
import Data.Foldable as F
import Data.Int
import Data.Text
import Options.Applicative
import Quine.Exception
import System.IO
import System.Process
import System.Remote.Monitoring
import qualified System.Remote.Gauge as G
import qualified System.Remote.Counter as C
import qualified System.Remote.Label as L

class Setting t a | t -> a where
  assign :: MonadIO m => t -> a -> m ()        -- set
  assign _ _ = return ()

class Updating t a | t -> a where
  update :: MonadIO m => t -> (a -> a) -> m () -- modify
  update _ _ = return ()

class Num a => Gauged t a | t -> a where
  dec :: MonadIO m => t -> m ()
  sub :: MonadIO m => t -> a -> m ()

class Incremental t where
  inc :: MonadIO m => t -> m ()
  inc _ = return ()

  add :: MonadIO m => t -> Int64 -> m ()
  add _ _ = return ()

-- | Enable/disable EKG

data MonitorOptions = MonitorOptions
  { _monitorHost    :: String
  , _monitorPort    :: Int
  , _monitorEnabled :: Bool
  , _monitorOpen    :: Bool
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy ''MonitorOptions

monitorUri :: HasMonitorOptions t => t -> String
monitorUri t = "http://" ++ t^.monitorHost ++ ":" ++ show (t^.monitorPort) ++ "/"

-- | Parse EKG configuration
parseMonitorOptions :: Parser MonitorOptions
parseMonitorOptions = MonitorOptions
  <$> strOption (long "ekg-host" <> short 'H' <> help "host for the EKG server" <> metavar "HOST" <> action "hostname" <> value "localhost")
  <*> option auto (long "ekg-port" <> short 'P' <> help "port for the EKG server" <> metavar "PORT" <> value 5616)
  <*> (not <$> switch (long "no-ekg" <> short 'Q' <> help "do NOT start the EKG server"))
  <*> switch (long "ekg-open" <> short 'M' <> help "open EKG on launch")

instance Default MonitorOptions where
  def = MonitorOptions "localhost" 5616 True False

-- | Enable/disable EKG

data Monitor = Monitor
  { __monitorOptions :: MonitorOptions
  , _monitorServer   :: Maybe Server
  } deriving Typeable

makeClassy ''Monitor

instance HasMonitorOptions Monitor where
  monitorOptions = _monitorOptions

withServer :: HasMonitor t => t -> (Server -> IO ()) -> IO ()
withServer t = F.forM_ $ t^.monitorServer

newtype Gauge = Gauge { runGauge :: Maybe G.Gauge } deriving Typeable
newtype Label = Label { runLabel :: Maybe L.Label } deriving Typeable
newtype Counter = Counter { runCounter :: Maybe C.Counter } deriving Typeable

instance Setting Label String where
  assign (Label t) a = liftIO $ maybe (return ()) (L.set ?? pack a) t

instance Updating Label String where
  update (Label t) f = liftIO $ maybe (return ()) (L.modify (pack . f . unpack)) t

instance Setting Gauge Int64 where
  assign (Gauge t) a = liftIO $ maybe (return ()) (G.set ?? a) t

instance Gauged Gauge Int64 where
  dec (Gauge t)   = liftIO $ maybe (return ()) G.dec t
  sub (Gauge t) i = liftIO $ maybe (return ()) (G.add ?? negate i) t

instance Incremental Gauge where
  inc (Gauge t)   = liftIO $ maybe (return ()) G.inc t
  add (Gauge t) i = liftIO $ maybe (return ()) (G.add ?? i) t

instance Incremental Counter where
  inc (Counter t)   = liftIO $ maybe (return ()) C.inc t
  add (Counter t) i = liftIO $ maybe (return ()) (C.add ?? i) t

gauge :: (MonadIO m, HasMonitor t) => Text -> t -> m Gauge
gauge = runReaderT . gaugeM

counter :: (MonadIO m, HasMonitor t) => Text -> t -> m Counter
counter = runReaderT . counterM

label :: (MonadIO m, HasMonitor t) => Text -> t -> m Label
label = runReaderT . labelM

-- | create a gauge
gaugeM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Gauge
gaugeM l = view monitorServer >>= maybe (return $ Gauge Nothing) (liftIO . fmap (Gauge . Just) . getGauge l)

-- | create a counter
counterM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Counter
counterM l = view monitorServer >>= maybe (return $ Counter Nothing) (liftIO . fmap (Counter . Just) . getCounter l)

-- | create a label
labelM :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Label
labelM t = view monitorServer >>= maybe (return $ Label Nothing) (liftIO . fmap (Label . Just) . getLabel t)

withMonitor :: HasMonitorOptions t => t -> (Monitor -> IO a) -> IO a
withMonitor t k
  | t^.monitorEnabled = do
    server <- forkServer (t^.monitorHost.packedChars) (t^.monitorPort)
    let uri = monitorUri t
    hPutStrLn stderr $ "Monitoring enabled at " ++ uri
    when (t^.monitorOpen) $ do
      -- TODO: check to see if we're on a mac first
      _ <- system $ "/usr/bin/open " ++ uri
      return ()
    k (Monitor (t^.monitorOptions) $ Just server) `finally` throwTo (serverThreadId server) Shutdown
  | otherwise = k $ Monitor (t^.monitorOptions) Nothing
