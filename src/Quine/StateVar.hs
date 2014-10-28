module Quine.StateVar
  ( (&=)
  , (&=!)
  , (&~)
  , (&~!)
  , the
  ) where

import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL.GL.StateVar

infixr 2 &=, &=!, &~, &~!

(&=) :: (MonadIO m, HasSetter s) => s a -> a -> m ()
v &= a = liftIO (v $= a)

(&=!) :: (MonadIO m, HasSetter s) => s a -> a -> m ()
v &=! a = liftIO (v $=! a)

(&~) :: (MonadIO m, HasSetter s, HasGetter s) => s a -> (a -> a) -> m ()
v &~ a = liftIO (v $~ a)

(&~!) :: (MonadIO m, HasSetter s, HasGetter s) => s a -> (a -> a) -> m ()
v &~! a = liftIO (v $~! a)

the :: MonadIO m => HasGetter s => s a -> m a
the v = liftIO $ get v
