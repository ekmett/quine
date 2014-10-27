{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Monitor.Exception
  ( ShutdownMonitor(..)
  ) where

import Control.Exception
import Data.Data

data ShutdownMonitor = ShutdownMonitor deriving (Show,Typeable,Data)

instance Exception ShutdownMonitor
