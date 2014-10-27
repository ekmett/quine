-- | Start to make SDL binding a bit prettier
--
-- So far this is just Graphics.UI.SDL.Basic minus log handling and hints
-- plus a few other things 
module Quine.SDL.Basic
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
import Control.Monad.IO.Class
import Data.Functor
import Data.Version as Data
import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Prelude hiding (init)
import Quine.SDL.Exception

-- * Initialization

init :: MonadIO m => SDL.InitFlag -> m ()
init x = liftIO (SDL.init x >>= err)

initSubSystem :: MonadIO m => SDL.InitFlag -> m ()
initSubSystem x = liftIO (SDL.initSubSystem x >>= err)

quit :: MonadIO m => m ()
quit = liftIO SDL.quit

quitSubSystem :: MonadIO m => SDL.InitFlag -> m ()
quitSubSystem = liftIO . SDL.quitSubSystem

wasInit :: MonadIO m => SDL.InitFlag -> m SDL.InitFlag
wasInit = liftIO . SDL.wasInit

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
