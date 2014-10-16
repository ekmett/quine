{-# LANGUAGE DeriveDataTypeable #-}
-- | Start to make SDL binding a bit prettier
--
-- So far this is just Graphics.UI.SDL.Basic minus log handling and hints
-- plus a few other things 
module Engine.SDL.Exception
  ( 
  -- * Extensible Exceptions
    SDLException(..)
  -- * Utilities
  , err
  ) where

import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Prelude hiding (init)

data SDLException = SDLException String
  deriving (Show, Typeable)

instance Exception SDLException

err :: CInt -> IO ()
err e 
  | e < 0 = do
    msg <- SDL.getError >>= peekCString
    SDL.clearError
    throw $ SDLException msg
  | otherwise = return ()
