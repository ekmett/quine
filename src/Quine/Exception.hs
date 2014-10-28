{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Exception
  ( Shutdown(..)
  , _Shutdown
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Data.Data
import GHC.Generics

data Shutdown = Shutdown deriving (Show,Typeable,Data,Generic)
instance Exception Shutdown

_Shutdown :: Prism' SomeException Shutdown
_Shutdown = exception
