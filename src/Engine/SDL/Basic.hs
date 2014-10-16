{-# LANGUAGE DeriveDataTypeable #-}
-- | Start to make SDL binding a bit prettier
--
-- So far this is just Graphics.UI.SDL.Basic minus log handling and hints
-- plus a few other things 
module Engine.SDL.Basic
  ( 
  -- * Initialization and Shutdown
  -- ** Subsystems
    initFlagTimer
  , initFlagAudio
  , initFlagVideo
  , initFlagJoystick
  , initFlagHaptic
  , initFlagGameController
  , initFlagEvents
  , initFlagEverything
  -- * Initialization
  , init
  , initSubSystem
  , quit
  , quitSubSystem
  , wasInit
  -- * Versioning
  , version
  , revision
  , revisionNumber
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.Version as Data
import Engine.SDL.Exception
import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL 
  ( quit
  , quitSubSystem
  , wasInit
  , initFlagTimer
  , initFlagAudio
  , initFlagVideo
  , initFlagJoystick
  , initFlagHaptic
  , initFlagGameController
  , initFlagEvents
  , initFlagEverything
  )
import Prelude hiding (init)

-- * Initialization


init :: Word32 -> IO ()
init = SDL.init >=> err

initSubSystem :: Word32 -> IO ()
initSubSystem = SDL.initSubSystem >=> err

-- * Version

-- | Get the Version (and Revision)
version :: MonadIO m => m Data.Version
version = liftIO $ alloca $ \p -> do
  SDL.getVersion p
  SDL.Version x y z <- peek p
  w <- revisionNumber
  return $ Data.Version (fromIntegral <$> [fromIntegral x,fromIntegral y,fromIntegral z, w]) []

revision :: MonadIO m => m String
revision = liftIO $ SDL.getRevision >>= peekCString

revisionNumber :: MonadIO m => m Int
revisionNumber = liftIO $ fromIntegral <$> SDL.getRevisionNumber
