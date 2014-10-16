{-# LANGUAGE DeriveDataTypeable #-}
-- | Start to make SDL binding a bit prettier
--
-- So far this is just Graphics.UI.SDL.Basic minus log handling and hints
-- plus a few other things 
module Engine.SDL.Basic
  ( 
  -- * Initialization and Shutdown
    init
  , initSubSystem
  , SDL.quit
  , SDL.quitSubSystem
  , SDL.wasInit
  -- * Versioning
  , getVersion
  , getRevision
  , getRevisionNumber
  ) where

import Control.Monad
import Data.Functor
import Data.Version as Data
import Engine.SDL.Exception
import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Prelude hiding (init)

-- * Initialization
init :: Word32 -> IO ()
init = SDL.init >=> err

initSubSystem :: Word32 -> IO ()
initSubSystem = SDL.initSubSystem >=> err

-- * Version

-- | Get the Version (and Revision)
getVersion :: IO Data.Version
getVersion = alloca $ \p -> do
  SDL.getVersion p
  SDL.Version x y z <- peek p
  w <- getRevisionNumber
  return $ Data.Version (fromIntegral <$> [fromIntegral x,fromIntegral y,fromIntegral z, w]) []
  
getRevision :: IO String
getRevision = SDL.getRevision >>= peekCString

getRevisionNumber :: IO Int
getRevisionNumber = fromIntegral <$> SDL.getRevisionNumber
