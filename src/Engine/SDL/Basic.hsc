-- | Start to make SDL binding a bit prettier
--
-- So far this is just Graphics.UI.SDL.Basic minus log handling and hints
-- plus a few other things 
module Engine.SDL.Basic
  ( 
  -- * Initialization and Shutdown
    init
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
import Data.Functor
import Data.Version as Data
import Engine.SDL.Exception
import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Prelude hiding (init)

-- * Initialization

init :: SDL.InitFlag -> IO ()
init = SDL.init >=> err

initSubSystem :: SDL.InitFlag -> IO ()
initSubSystem = SDL.initSubSystem >=> err

quit :: IO ()
quit = SDL.quit

quitSubSystem :: SDL.InitFlag -> IO ()
quitSubSystem = SDL.quitSubSystem

wasInit :: SDL.InitFlag -> IO SDL.InitFlag
wasInit = SDL.wasInit

-- * Version

-- | Get the Version (and Revision)
version :: IO Data.Version
version = alloca $ \p -> do
  SDL.getVersion p
  SDL.Version x y z <- peek p
  w <- revisionNumber
  return $ Data.Version (fromIntegral <$> [fromIntegral x,fromIntegral y,fromIntegral z, w]) []

revision :: IO String
revision = SDL.getRevision >>= peekCString

revisionNumber :: IO Int
revisionNumber = fromIntegral <$> SDL.getRevisionNumber
