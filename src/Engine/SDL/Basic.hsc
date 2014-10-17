{-# LANGUAGE DeriveDataTypeable, PatternSynonyms #-}
-- | Start to make SDL binding a bit prettier
--
-- So far this is just Graphics.UI.SDL.Basic minus log handling and hints
-- plus a few other things 
module Engine.SDL.Basic
  ( 
  -- * Initialization and Shutdown
  -- ** SubSystems
    pattern InitFlagTimer
  , pattern InitFlagAudio
  , pattern InitFlagVideo
  , pattern InitFlagJoystick
  , pattern InitFlagHaptic
  , pattern InitFlagGameController
  , pattern InitFlagEvents
  , pattern InitFlagEverything
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

#include "SDL.h"

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
  )
import Prelude hiding (init)

-- * Initialization

type SubSystem = Word32

init :: SubSystem -> IO ()
init = SDL.init >=> err

initSubSystem :: SubSystem -> IO ()
initSubSystem = SDL.initSubSystem >=> err

pattern InitFlagTimer = (#const SDL_INIT_TIMER)
pattern InitFlagAudio = (#const SDL_INIT_AUDIO)
pattern InitFlagVideo = (#const SDL_INIT_VIDEO)
pattern InitFlagJoystick = (#const SDL_INIT_JOYSTICK)
pattern InitFlagHaptic = (#const SDL_INIT_HAPTIC)
pattern InitFlagGameController = (#const SDL_INIT_GAMECONTROLLER)
pattern InitFlagEvents = (#const SDL_INIT_EVENTS)
-- pattern InitFlagNoParachute = (#const SDL_INIT_NOPARACHUTE)
pattern InitFlagEverything = (#const SDL_INIT_EVERYTHING)

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
