{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- simulation is a bit of a shell game, we need two physics states per rendering frame
-- but we need two rendering frames for temporal antialiasing, so we'll have up to 4
-- physics states in flight at any given time.
--
-- Here we track the two current states
--------------------------------------------------------------------
module Quine.Simulation
  ( Simulation(..)
  , HasSimulation(..)
  , simulationFPS
  , simulationSPF
  , simulationBurstRate
  , createSimulation
  , simulate
  , Simulated(..)
  ) where

import Control.Comonad
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Lens
import Data.Default
#if ! MIN_VERSION_base(4,8,0)
import Data.Foldable
#endif
import Quine.Clock
import Quine.Meter
import Quine.Monitor
import Quine.Ref

-- | # of fps
simulationFPS :: Double
simulationFPS = 60

simulationSPF :: DeltaTime
simulationSPF = recip simulationFPS

-- | Catch up rate, if we're at less than a frame every second, we're hurting!
simulationBurstRate :: Int
simulationBurstRate = 60

data Simulation a = Simulation
  { _simulationStart        :: !Time    -- environment?
  , _simulationTime         :: !Time
  , _simulationFrameCounter :: !Counter -- as well?
  , _simulationOld          :: !(Ref a)
  , _simulationState        :: !(Ref a)
  , _simulationMeter        :: !Meter
  } deriving (Functor, Foldable, Traversable)

makeClassy ''Simulation

class Simulated a where
  newState    :: MonadIO m => a -> a -> m a
  deleteState :: MonadIO m => a -> m ()

instance Simulated () where
  newState () () = return ()
  deleteState = return

instance (Simulated a, Simulated b) => Simulated (a, b) where
  newState (ao,bo) (ac,bc) = liftM2 (,) (newState ao ac) (newState bo bc)
  deleteState (ao,bo)      = deleteState ao >> deleteState bo

createSimulation :: Simulated a => Monitor -> a -> a -> IO (Simulation a)
createSimulation ekg a b = do
  s  <- now
  ff <- counter "physics.frame" ekg
  ra <- newRef (deleteState a) a
  rb <- newRef (deleteState b) b
  return $ Simulation s s ff ra rb def

-- run up to a burst worth of simulation frames w/ interleaved polling
simulate :: (MonadIO m, MonadState s m, HasSimulation s a, Simulated a) => m () -> m (Double, Time)
simulate poll = go simulationBurstRate
 where
  go b = do
    poll -- run this between physics frames
    t <- now
    Simulation t0 tn ff ro rc m <- use simulation
    let tn' = tn + simulationSPF
    if b > 0 && tn' <= t
      then do
        rn <- newState (extract ro) (extract rc) >>= \n -> newRef (deleteState n) n
        releaseRef ro
        inc ff
        simulation .= Simulation t0 tn' ff rc rn (tick tn' m)
        go $! b - 1
      else return $ ((t-tn) * simulationFPS, t - t0)
