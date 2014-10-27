{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Engine.Shutdown
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
