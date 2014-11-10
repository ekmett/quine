{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | simulation is a bit of a shell game, we need two physics states per rendering frame
-- but we need two rendering frames for temporal antialiasing, so we'll have up to 4
-- physics states in flight at any given time.
--
-- Here we track the two current states
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
import Data.Foldable
import Quine.Clock
import Quine.Ref

-- | 25 fps
simulationFPS :: Double
simulationFPS = 25

simulationSPF :: DeltaTime
simulationSPF = recip simulationFPS

-- | Catch up, up to 25 frames at a time
simulationBurstRate :: Int
simulationBurstRate = 25

data Simulation a = Simulation
  { _simulationStart :: !Time
  , _simulationTime  :: !Time
  , _simulationOld   :: !(Ref a)
  , _simulationState :: !(Ref a)
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

createSimulation :: Simulated a => a -> a -> IO (Simulation a)
createSimulation a b = do
  s  <- now
  ra <- newRef (deleteState a) a
  rb <- newRef (deleteState b) b
  return $ Simulation s s ra rb

-- run up to a burst worth of simulation frames w/ interleaved polling
simulate :: (MonadIO m, MonadState s m, HasSimulation s a, Simulated a) => m () -> m ()
simulate poll = go simulationBurstRate
 where
  go b = do
    poll -- run this between physics frames
    t <- now 
    Simulation t0 tn ro rc <- use simulation 
    let tn' = tn + simulationSPF
    when (b > 0 && tn' <= t) $ do
      rn <- newState (extract ro) (extract rc) >>= \n -> newRef (deleteState n) n
      releaseRef ro
      simulation .= Simulation t0 tn' rc rn
      go $! b - 1
