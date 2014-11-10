{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
module Quine.System
  ( System(..)
  , HasSystem(..)
  ) where

import Control.Lens
import Quine.Display
import Quine.Input
import Quine.Camera
import Quine.Simulation

data System a = System
  { _systemDisplay    :: Display
  , _systemInput      :: Input
  , _systemCamera     :: Camera
  , _systemSimulation :: Simulation a
  }

makeLenses ''System

class (HasDisplay t, HasInput t, HasCamera t, HasSimulation t a) => HasSystem t a | t -> a where
  system :: Lens' t (System a)

instance HasDisplay (System a) where
  display = systemDisplay

instance HasInput (System a) where
  input = systemInput

instance HasCamera (System a) where
  camera = systemCamera

instance HasSimulation (System a) a where
  simulation = systemSimulation

instance HasSystem (System a) a where
  system = id
